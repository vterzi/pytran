! This module contains types of lists and iterators for these lists.
!
! # Notation
!
! In the following documentation keywords surrounded by angle brackets ("<" and ">") are placeholders, and optional
! parts are surrounded by braces ("[" and "]").
!
! # Lists
!
! Name of a list type is `<type>[<kind>][Array]List_t`, where <type> is the element type, and <kind> is the element kind
! (number of bytes).  If <kind> is omitted, the default kind is used.  For LOGICAL type only the list type with the
! default kind is available.  The list types with infix `Array` are lists with same-size arrays as list elements and are
! available only for logical and numerical types (LOGICAL, INTEGER, REAL, COMPLEX).  The array lists are initialized
! with the constructor `<type>[<kind>]ArrayList_t(<size>)`, where <size> is the desired array size of a list element.
! Any list can be initialized by assigning an array or another list with the same element type and kind.
!
! Available type-bound procedures of a list type:
! * `Dimension()`: function of array lists, returns array size of list elements
! * `Len()`: function of character lists, returns greatest character length of list elements
! * `Size()`: function, returns number of list elements
! * `Append(<element(s)>)`: subroutine, appends a value, an array, or a list to the end of the list
! * `Insert(<index>,<element(s)>)`: subroutine, inserts a value, an array, or a list at given index in the list
! * `Delete(<start>[,<stop>[,<step>]])`: subroutine, deletes an element or a slice from the list
! * `Get(<index>)`: function, returns the value of the element at a given index in the list
! * `Set(<index>,<value>)`: subroutine, sets the value of the element at a given index in the list
! * `Add(<index>,<value>)`: subroutine of numerical lists, adds a value (can be an array in array lists) to the element
!                           at a given index in the list
! * `Multiply(<index>,<value>)`: subroutine of numerical lists, multiplies the element at a given index in the list by a
!                                value (can be an array in array lists)
! * `Concatenate(<index>,<value>)`: subroutine of character lists, concatenates a value to the element at a given index
!                                   in the list
! * `Find(<value>[,<reversed>])`: function, returns the first found index of the element with the given value, if the
!                               second argument is gvien and is TRUE, search is performed in the reversed order
! * `Contains(<value>)`: function, returns TRUE if the given value is contained in the list and FALSE otherwise
! * `Extend(<value>)`: subroutine, appends a value to the end of the list if it is not contained in the list
! * `ToArray()`: function, returns list elements as an array (elements of an array list become columns in a 2D array)
! * `Empty()`: subroutine, empties the list
!
! # Iterators
!
! Iterator types follow the same naming rules as list types but with `Iter_t` suffix instead of `List_t` suffix.
! Iterators are initialized with the constructor `<type>[<kind>][Array]Iter_t(<list>[,<start>])`, where <list> is the
! the list with the same element type and kind as the iterator, and <start> is the starting index.  If <start> is
! omitted, iterator starts with the first list element.
!
! Available type-bound procedures of an iterator type:
! * `Get()`: function, returns the value of the list element at the current position of the iterator
! * `Set(<value>)`: subroutine, sets the value of the list element at the current position of the iterator
! * `Step()`: subroutine, moves the position of the iterator to the next list element
! * `Next()`: function, returns the value of the list element at the current position of the iterator and moves the
!             position to the next list element


#define _(X) X

#define _CONCAT(X,Y) _(X)_(_)_(Y)
#define _NODE _(_TYPE_NAME)_(_KIND_LABEL)_(_ARRAY_LABEL)_(Node)
#define _ITER _(_TYPE_NAME)_(_KIND_LABEL)_(_ARRAY_LABEL)_(Iter)
#define _LIST _(_TYPE_NAME)_(_KIND_LABEL)_(_ARRAY_LABEL)_(List)
#define _PROC(T,P) _CONCAT(T,P)
#define _SUBPROC(T,P,X) _(T)_(_)_(P)_(X)
#define _CONSTRUCTOR(T) _CONCAT(New,T)


module stdlib_list
    use stdlib_kinds

    implicit none

    private

#define _FILE "../list/type.inc"
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _CHARACTER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ARRAY
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/defs.inc"
#undef _ARRAY
#undef _FILE

contains
#define _FILE "../list/proc.inc"
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _CHARACTER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ARRAY
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/defs.inc"
#undef _ARRAY
#undef _FILE
end module stdlib_list
