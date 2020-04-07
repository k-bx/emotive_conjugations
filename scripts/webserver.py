from flask import Flask, request
import spacy

app = Flask(__name__)
nlp = spacy.load("en_core_web_lg")

@app.route('/ping')
def pong():
    return 'pong!'

@app.route('/api/call', methods=['POST'])
def spacy_ner():
    args = request.get_json()
    if args['tag'] == 'cmd_spacy_ner':
        text = args['contents']['text']

        doc = nlp(text)
        rv = {
            "ents": []
        }
        for ent in doc.ents:
            rv["ents"].append({
                "text": ent.text,
                "start": ent.start,
                "start_char": ent.start_char,
                "end": ent.end,
                "end_char": ent.end_char,
                "label_": ent.label_
            })
        return rv
    raise ValueError('Bad request: ' + args)
