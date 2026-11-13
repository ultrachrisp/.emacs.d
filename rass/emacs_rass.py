"""Rassumfrassum logic class for Emacs's Eglot TS/JS setup (see config.org).

Loaded by rass through `--logic-class emacs_rass.EmacsTypeScriptLogic`,
with this directory on PYTHONPATH.
"""

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
