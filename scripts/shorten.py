#!/usr/bin/env python3
import re
from sys import argv,stderr

def is_site(link: str, site: str):
    return re.match(f'^https?:\/\/{site}.*', link) is not None

if __name__ == "__main__":
    if len(argv) != 2:
        print(f'Usage: {argv[0]} <link>.', file=stderr)
        exit(1)
    link = argv[1]
    matches = re.search(r'https?://w{3}?\.?amazon.(\w{1,5})/.*(\/?dp\/[^/]*)', link)
    if matches:
        print(f'https://amazon.{matches.group(1)}/{matches.group(2)}')
    elif is_site(link, 'youtube\.com'):
        matches = re.search(r'\?v=(.*)', link)
        if matches:
            print(f'https://youtu.be/{matches.group(1)}')
        else:
            print('Invalid youtube product link.')
    else:
        print('Unidentifiable link.', file=stderr)
