# import sys
from flask import Flask, request
import spacy

app = Flask(__name__)
nlp = spacy.load("en_core_web_lg")
# nlp = spacy.load("en_core_web_sm")

@app.route('/ping')
def pong():
    return 'pong!'

@app.route('/api/call', methods=['POST'])
def spacy_ner():
    args = request.get_json()
    # print('> args[tag]: ', args['tag'], file=sys.stderr)
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
    elif args['tag'] == 'cmd_spacy_pos':
        text = args['contents']['text']

        doc = nlp(text)
        rv = {
            "tokens": []
        }
        for token in doc:
            rv["tokens"].append({
                "text": token.text,
                "lemma_": token.lemma_,
                "pos_": token.pos_,
                "tag_": token.tag_,
                "dep_": token.dep_,
                "shape_": token.shape_,
                "is_alpha": token.is_alpha,
                "is_ascii": token.is_ascii,
                "is_digit": token.is_digit,
                "is_punct": token.is_punct,
                "is_left_punct": token.is_left_punct,
                "is_right_punct": token.is_right_punct,
                "is_space": token.is_space,
                "is_bracket": token.is_bracket,
                "is_quote": token.is_quote,
                "is_currency": token.is_currency,
                "like_url": token.like_url,
                "like_num": token.like_num,
                "like_mail": token.like_email,
                "is_oov": token.is_oov,
                "is_stop": token.is_stop,
                "head_i": token.head.i,
                "left_edge_i": token.left_edge.i,
                "right_edge_i": token.right_edge.i,
                "i": token.i,
                "ent_type_": token.ent_type_,
                "ent_iob_": token.ent_iob_,
                "ent_kb_id": token.ent_kb_id,
                "ent_kb_id_": token.ent_kb_id_,
                "norm_": token.norm_,
                "lang_": token.lang_,
                "prob": token.prob,
                "idx": token.idx,
                "sentiment": token.sentiment,
                "lex_id": token.lex_id,
                "rank": token.rank,
                "cluster": token.cluster,
            })
        return rv

    raise ValueError('Bad request: ' + str(args['tag']))
