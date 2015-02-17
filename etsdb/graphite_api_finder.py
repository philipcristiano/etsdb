import re

import requests
from graphite_api.node import LeafNode, BranchNode

class Finder(object):
    def __init__(self, config):
        print config

    def find_nodes(self, query):
        req = requests.get('http://127.0.0.1:8080/metrics')
        metrics = req.json()
        for metric in metrics:
            search = re.search(query.pattern, metric)
            if search:
                print metric



class QueryMatcher(object):

    def __init__(self, query):
        self._query = query.split('.')

    def matches(self, metric):
        parts = metric.split('.')
        if len(parts) != len(self._query):
            return False
        for q, m in zip(self._query, parts):
            if q == '*' or q == m:
                continue
            else:
                return False
        return True
