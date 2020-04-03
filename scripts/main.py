import json
import sys

from newspaper import Article


def main():
    print('> sys.args: ', sys.argv, file=sys.stderr)
    args_path = sys.argv[1]
    with open(args_path) as args_fp:
        args_raw = args_fp.read()
        args = json.loads(args_raw)
        print('> args_raw: ', args_raw[:300], file=sys.stderr)
        if args['tag'] == 'cmd_ping':
            print('pong')
        elif args['tag'] == 'cmd_parse_article':
            html = args['contents']['html']

            article = Article('')
            article.set_html(html)
            article.parse()
            print('> title', article.title, file=sys.stderr)
            print('> authors', article.authors, file=sys.stderr)
            print('> publish date', article.publish_date.strftime('%Y-%m-%d %H:%M:%S') if article.publish_date else None,
                  file=sys.stderr)
            # print('> description', article.meta_description[:100], file=sys.stderr)
            # print('> text', article.text[:100], file=sys.stderr)
            res = {
                "title": article.title,
                "authors": article.authors,
                "pub_date": article.publish_date.timestamp() if article.publish_date else None,
                "description": article.meta_description,
                "text": article.text,
                "language": article.meta_lang
            }
            print(json.dumps(res))


if __name__ == '__main__':
    main()
