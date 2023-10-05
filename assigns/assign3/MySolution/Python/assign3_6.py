from typing import TypeVar, Generic, Callable

A = TypeVar('A')

class MyListClass(Generic[A]):
    def __init__(self, mylist: 'mylist'):
        self.mylist = mylist

    def foreach(self, func: Callable[[A], None]) -> None:
        current = self.mylist
        while current:
            if isinstance(current, Cons):
                func(current.head)
                current = current.tail
            else:  # Nil
                break

    def rforeach(self, func: Callable[[A], None]) -> None:
        current = self.mylist
        prev = None
        while current:
            if isinstance(current, Cons):
                if not current.tail:
                    func(current.head)
                    break
                else:
                    prev = current
                    current = current.tail
            else:  # Nil
                if prev:
                    func(prev.head)
                break
