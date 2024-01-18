      ! reduced tc from defect 355111
      TYPE Base  (len1)
        INTEGER, LEN :: len1
      END TYPE Base

      TYPE, EXTENDS(Base) :: List
        TYPE(List(len1)), POINTER :: Next => NULL()
        CLASS(*), ALLOCATABLE :: ITEM
      END TYPE List

      TYPE(List(10)), POINTER :: Init

      END PROGRAM
