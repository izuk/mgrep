# Overview

MGrep scans a file for lines containing any of a list of substrings.

# Usage

    # Find users in data files.
    $ mgrep users data1 data2 data2

    # Filter a whitelist out of a log stream.
    $ stream-log | mgrep -v ip.whitelist

# Aho-Corasick

We use the Aho-Corasick algorithm to support matching against large
substring sets.  This was the main motivation for mgrep: standard grep
against more than 20 patterns becomes irritatingly slow, even when the
patterns are combined.  mgrep really is O(n) in the size of the files
to scan.

One particularly nice feature of this lazy implementation of
Aho-Corasick is that portions of the trie that are never visited are
never constructed.  This may have nice benefits for large substring
sets.