class MyList:
    def foreach(self, work):
        pass
    
    def rforeach(self, work):
        pass

class MyNil(MyList):
    def __repr__(self):
        return "MyNil()"

class MyCons(MyList):
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def foreach(self, work):
        work(self.head)
        self.tail.foreach(work)

    def rforeach(self, work):
        self.tail.rforeach(work)
        work(self.head)

    def __repr__(self):
        return f"MyCons({self.head}, {self.tail})"

class MySnoc(MyList):
    def __init__(self, lst, last_elem):
        self.lst = lst
        self.last_elem = last_elem

    def foreach(self, work):
        self.lst.foreach(work)
        work(self.last_elem)

    def rforeach(self, work):
        work(self.last_elem)
        self.lst.rforeach(work)

    def __repr__(self):
        return f"MySnoc({self.lst}, {self.last_elem})"

class MyReverse(MyList):
    def __init__(self, lst):
        self.lst = lst

    def foreach(self, work):
        self.lst.rforeach(work)

    def rforeach(self, work):
        self.lst.foreach(work)

    def __repr__(self):
        return f"MyReverse({self.lst})"

class MyAppend2(MyList):
    def __init__(self, lst1, lst2):
        self.lst1 = lst1
        self.lst2 = lst2

    def foreach(self, work):
        self.lst1.foreach(work)
        self.lst2.foreach(work)

    def rforeach(self, work):
        self.lst2.rforeach(work)
        self.lst1.rforeach(work)

    def __repr__(self):
        return f"MyAppend2({self.lst1}, {self.lst2})"

def mylist_nil():
    return MyNil()

def mylist_cons(x, lst):
    return MyCons(x, lst)

def mylist_snoc(lst, x):
    return MySnoc(lst, x)

def mylist_reverse(lst):
    return MyReverse(lst)

def mylist_append2(lst1, lst2):
    return MyAppend2(lst1, lst2)

def mylist_foreach(xs, work):
    xs.foreach(work)

def mylist_rforeach(xs, work):
    xs.rforeach(work)

def foreach_to_map_pylist(foreach):
    def map_pylist(xs, fopr_func):
        res = []
        def work_func(x0):
            nonlocal res
            res.append(fopr_func(x0))
        foreach(xs, work_func)
        return res
    return map_pylist
