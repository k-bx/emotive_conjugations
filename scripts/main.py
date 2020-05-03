import json
import sys
import datetime

from newspaper import Article
import newsplease


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
            url = args['contents']['url']

            article = Article(url)
            article.set_html(html)
            article.parse()
            print('> title', article.title, file=sys.stderr)
            print('> authors', article.authors, file=sys.stderr)
            print('> publish date', article.publish_date.strftime('%Y-%m-%d %H:%M:%S') if article.publish_date else None,
                  file=sys.stderr)
            res = {
                "title": article.title,
                "authors": article.authors,
                "pub_date": article.publish_date.timestamp() if article.publish_date else None,
                "description": article.meta_description,
                "text": article.text,
                "language": article.meta_lang
            }
            print(json.dumps(res))
        elif args['tag'] == 'cmd_parse_news_please':
            html = args['contents']['html']
            url = args['contents']['url']
            download_date = (
                datetime.datetime.fromtimestamp(args['contents']['download_date']))

            article = newsplease.NewsPlease.from_html(html, url, download_date.strftime('%Y-%m-%d %H:%M:%S'))
            res = {
                "authors": article.authors,
                "date_download": article.date_download.timestamp() if article.date_download else None,
                "date_publish": article.date_publish.timestamp() if article.date_publish else None,
                "date_modify": article.date_modify.timestamp() if article.date_modify else None,
                "description": article.description,
                "filename": article.filename,
                "image_url": article.image_url,
                "language": article.language,
                "localpath": article.localpath,
                "title": article.title,
                "title_page": article.title_page,
                "title_rss": article.title_rss,
                "source_domain": article.source_domain,
                "maintext": article.maintext,
                "url": article.url,
            }
            print(json.dumps(res))
        elif args['tag'] == 'cmd_download_url':
            url = args['contents']['url']

            article = newsplease.NewsPlease.from_url(url)
            res = {
                "authors": article.authors,
                "date_download": article.date_download.timestamp() if article.date_download else None,
                "date_publish": article.date_publish.timestamp() if article.date_publish else None,
                "date_modify": article.date_modify.timestamp() if article.date_modify else None,
                "description": article.description,
                "filename": article.filename,
                "image_url": article.image_url,
                "language": article.language,
                "localpath": article.localpath,
                "title": article.title,
                "title_page": article.title_page,
                "title_rss": article.title_rss,
                "source_domain": article.source_domain,
                "maintext": article.maintext,
                "url": article.url,
            }
            print(json.dumps(res))


if __name__ == '__main__':
    main()
