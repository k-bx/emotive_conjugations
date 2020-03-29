import json
import sys

def main():
    print('> sys.args: ', sys.argv)
    args = json.loads(sys.argv[1])
    if args['cmd'] == 'ping':
        print('pong', file=sys.stderr)

if __name__ == '__main__':
    main()
