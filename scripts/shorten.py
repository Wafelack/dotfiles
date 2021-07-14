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
    if is_site(link, 'amazon\.com'):
        matches = re.search(r'\/dp\/[^/]*', link)
        if matches:
            print(f'https://amazon.com{matches.group(0)}')
        else:
            print('Invalid amazon product link.')
    elif is_site(link, 'youtube\.com'):
        matches = re.search(r'\?v=(.*)', link)
        if matches:
            print(f'https://youtu.be/{matches.group(1)}')
        else:
            print('Invalid youtube product link.')
    else:
        print('Unidentifiable link.', file=stderr)
