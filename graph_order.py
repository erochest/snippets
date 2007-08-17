

# This takes a dependency graph in the form
#     graph[node] -> [list of nodes depended upon]
# and returns a list of the nodes in order to make sure all dependencies
# are satisfied first before handling any dependent nodes.
#
# This throws an exception on circular dependencies.
#
# Essentially, this is a reversed topological sort of the graph.


from collections import deque


graph = {
    'a': ['b', 'c'],
    'b': ['e'],
    'c': [],
    'd': ['c'],
    'e': ['d'],
    'f': ['a', 'b', 'c', 'd', 'e'],
    }


def sort_graph(graph):
    working = deque()
    seen = set()
    in_order = []
    for node in graph:
        __sort_graph(graph, node, seen, working, in_order)
    return in_order


def __sort_graph(graph, node, seen, working, in_order):
    for in_stack in working:
        if in_stack == node:
            raise Exception('Circular reference: %r -> %r' % (working, node))
    if node in seen:
        return
    working.append(node)
    for edge in graph[node]:
        if edge not in seen:
            __sort_graph(graph, edge, seen, working, in_order)
    seen.add(node)
    in_order.append(node)
    working.pop()


def main():
    print sort_graph(graph)


if __name__ == '__main__':
    main()

