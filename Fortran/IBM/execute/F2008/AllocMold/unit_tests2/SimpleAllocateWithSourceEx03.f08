PROGRAM AllocateWithSourceExp03
      IMPLICIT NONE

      TYPE Base  (l1)
        INTEGER, LEN  :: l1 = 10
        INTEGER :: member=7
      END TYPE Base

       TYPE(Base(2)),pointer :: Arg1, Arg2
       INTEGER :: l1=2
       ALLOCATE (Arg1,Arg2, SOURCE=Base(l1)())

END PROGRAM AllocateWithSourceExp03
