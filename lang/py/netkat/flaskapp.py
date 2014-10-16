""" Construct a flask application that can serve as NetKAT application, given a
handler function. """

import json
from flask import Flask, request, make_response

def create(state, handler, name="netkat"):
    app = Flask(name)

    gs = { 'instances' : {}, 'counter' : 1 }

    @app.route('/netkat/app', methods=['POST'])
    def _create():
        gs['instances'][gs['counter']] = state()
        gs['counter'] = gs['counter'] + 1
        response = make_response("Created", 201)
        response.headers['Location'] = '/netkat/app/%s' % (gs['counter'] - 1)
        return response

    @app.route('/netkat/app/<int:_id>', methods=['PUT'])
    def _handle(_id):
        event = json.loads(request.data)
        result = handler(gs['instances'][_id], event)
        if result is None:
            return make_response("", 202)
        else:
            return make_response(result.dumps(), 200)

    return app
