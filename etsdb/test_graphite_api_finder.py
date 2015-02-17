import unittest

from graphite_api_finder import QueryMatcher

class TestGraphiteQueryMatcher(object):

    def test_single(self):
        m = QueryMatcher('foo.bar.baz')
        assert m.matches('foo.bar.baz')

    def test_end_glob(self):
        m = QueryMatcher('foo.bar.*')
        assert m.matches('foo.bar.baz')

    def test_middle_glob(self):
        m = QueryMatcher('foo.*.baz')
        assert m.matches('foo.bar.baz')

    def test_non_match(self):
        m = QueryMatcher('foo.bar.bar')
        assert not m.matches('foo.bar.baz')

    def test_uneven_length(self):
        m = QueryMatcher('foo.bar')
        assert not m.matches('foo.bar.baz')
