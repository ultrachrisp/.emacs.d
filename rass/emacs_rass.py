"""Rassumfrassum logic class for Emacs's Eglot TS/JS setup (see config.org).

Loaded by rass through `--logic-class emacs_rass.EmacsTypeScriptLogic`,
with this directory on PYTHONPATH.
"""

import asyncio

from rassumfrassum.presets.tslint import TypeScriptLogic


class EmacsTypeScriptLogic(TypeScriptLogic):
    """rass's TypeScript logic, made acceptable to TypeScript 7's server.

    `TypeScriptLogic` (from the bundled `tslint` preset) answers ESLint's
    workspace/configuration request, which ESLint needs to report anything.

    rass 0.3.5 sends textDocument/diagnostic with `previousResultId` set
    to null or -1 when it has no previous result. The LSP spec makes it an
    optional string, and TypeScript 7's server rejects anything else, so
    those pulls fail and its diagnostics never arrive. Drop the field
    unless it holds a real result id.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        request_server = self.request_server

        async def request_server_with_valid_params(server, method, payload):
            if (
                method == 'textDocument/diagnostic'
                and isinstance(payload, dict)
                and not isinstance(payload.get('previousResultId'), str)
            ):
                payload = {k: v for k, v in payload.items() if k != 'previousResultId'}
            return await request_server(server, method, payload)

        self.request_server = request_server_with_valid_params


class EmacsVueLogic(EmacsTypeScriptLogic):
    """Vue SFCs: vue-language-server first, vtsls second (plus Tailwind).

    vue-language-server 3.x (hybrid mode) asks the editor to run TypeScript
    requests for it with `tsserver/request` notifications, shaped
    [[id, command, args]], and waits for `tsserver/response` [[id, body]].
    Without a reply it stops reporting template diagnostics. Eglot can't
    answer these, so forward each one to vtsls (whose tsserver has
    @vue/typescript-plugin loaded) as a `typescript.tsserverRequest`
    command, and send the result back to the Vue server.
    """

    # Hover and signature help go to only the first server by default, and
    # the Vue server returns nothing for TypeScript symbols; vtsls (through
    # the Vue plugin) answers them in both <script> and <template>.
    _VTSLS_METHODS = ('textDocument/hover', 'textDocument/signatureHelp')

    async def on_client_request(self, method, params, servers):
        if method in self._VTSLS_METHODS:
            vtsls = [s for s in servers if 'vtsls' in s.name.lower()]
            if vtsls:
                return vtsls
        return await super().on_client_request(method, params, servers)

    async def on_server_notification(self, method, params, source):
        if method == 'tsserver/request' and 'vue' in source.name.lower():
            asyncio.ensure_future(self._forward_tsserver_requests(params, source))
            return
        await super().on_server_notification(method, params, source)

    async def _forward_tsserver_requests(self, params, vue_server):
        vtsls = next((s for s in self.servers.values()
                      if s is not vue_server and 'vtsls' in s.name.lower()), None)
        for req_id, command, args in params:
            body = None
            if vtsls is not None:
                is_error, result = await self.request_server(
                    vtsls, 'workspace/executeCommand',
                    {'command': 'typescript.tsserverRequest',
                     'arguments': [command, args]})
                if not is_error and isinstance(result, dict):
                    body = result.get('body')
            await self.notify_server(vue_server, 'tsserver/response', [[req_id, body]])
