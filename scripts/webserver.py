# import sys
import subprocess
import json
from pathlib import Path
from flask import Flask, request
import spacy
import fasttext
# from deep_learning_keras import SentimentAnalyser

app = Flask(__name__)
print('> loading spacy')
# nlp = spacy.load("en_core_web_md")
nlp = spacy.load("en_core_web_sm")
cfg = json.loads(subprocess.check_output([Path.home() / ".local/bin/dhall-to-json", "--file", Path.home() / 'conj.dhall']))
print('> loading amazon_review_full model')
amazon_review_full = fasttext.load_model(str(Path(cfg['cfgDataDir']) / 'fasttext' / 'amazon_review_full.ftz'))
# sentiment_keras_max_length = 200
# print('> loading en_vectors_web_lg')
# nlp2 = spacy.load("en_vectors_web_lg")
# print('> loading vocab')
# nlp2.vocab.from_disk(Path(cfg['cfgDataDir']) / 'spacy_keras_model' / 'vocab')
# print('> loading analyser')
# sentiment_analyser = SentimentAnalyser.load(
#     Path(cfg['cfgDataDir']) / 'spacy_keras_model',
#     nlp2,
#     max_length=sentiment_keras_max_length)


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
            rv["ents"].append(encode_span(ent))
        return rv
    elif args['tag'] == 'cmd_spacy_pos':
        text = args['contents']['text']

        doc = nlp(text)
        rv = {
            "tokens": [],
            "sents": [],
            "sent_sentiment": {}
        }
        for token in doc:
            rv["tokens"].append(encode_token(token))
        for sent in doc.sents:
            rv["sents"].append(encode_span(sent))
            # rv["sent_sentiment"][sent.start] = sentiment_analyser(sent)
        return rv
    elif args['tag'] == 'cmd_fasttext_sentiment_amazon':
        # print('> beginning')
        def label_to_float(lbl):
            """Convert a label to a float in [-1.0, 1.0] range"""
            i = int(lbl[len('__label__'):])
            if i == 1:
                return -1.0
            elif i == 2:
                return -0.5
            elif i == 3:
                return 0.0
            elif i == 4:
                return 0.5
            elif i == 5:
                return 1.0
            else:
                raise Exception('Impossible! Label out of range: ' + lbl)

        sentences = args['contents']['sentences']
        res = amazon_review_full.predict(sentences, threshold=0.5, k=1)
        rv = []
        for (labels, confidences) in zip(res[0], res[1]):
            if len(labels) > 0:
                rv.append(
                    dict(
                        label=label_to_float(labels[0]),
                        confidence=float(confidences[0]) if len(confidences) > 0 else None))
            else:
                rv.append(None)
        # print('> rv: ', rv)
        return json.dumps(rv)

    raise ValueError('Bad request: ' + str(args['tag']))


def encode_token(token):
    res = {
        "text": token.text,
        "orth": token.orth,
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
        "is_sent_start": token.is_sent_start,
    }
    return res

def encode_span(span):
    res = {
        "text": span.text,
        "start": span.start,
        "start_char": span.start_char,
        "end": span.end,
        "end_char": span.end_char,
        "label_": span.label_
    }
    return res


if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8082)
