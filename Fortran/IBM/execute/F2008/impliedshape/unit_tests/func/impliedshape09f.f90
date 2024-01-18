!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays of DTP
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape09f

      type base
        integer      :: a = 1
        character(4) :: b = 'abcd'
      end type

      type, extends(base) :: child(k,l,j)
         integer, kind :: k
         integer, len :: l,j

         character(l+j) :: c
         integer(k) :: i
      end type

      type(base), parameter :: t2 (*,*) = &
           & reshape([(base(i,'IJKL'),i=101,200)],[10,10])
      type(base), parameter :: t2a (10,10) = &
           & reshape([(base(i,'IJKL'),i=101,200)],[10,10])

      type(child(4,1,2)), parameter :: ta (*) = &
           & reshape([child(4,1,2)(c='abc',i=88), &
           & child(4,1,2)(10,'XLF ','xyz',44)],[2])
      type(child(4,1,2)), parameter :: taa (2) = &
           & reshape([child(4,1,2)(c='abc',i=88), &
           & child(4,1,2)(10,'XLF ','xyz',44)],[2])

      type(child(8,4,4)), parameter :: tb (*,*) = &
           & reshape([(child(8,4,4)(i,'ijkl','IJKLmnop',i),i=101,200)],[10,10])
      type(child(8,4,4)), parameter :: tba (10,10) = &
           & reshape([(child(8,4,4)(i,'ijkl','IJKLmnop',i),i=101,200)],[10,10])


      if (ANY(t2%a .NE. t2a%a) .OR. ANY(t2%b .NE. t2a%b)) then
        ERROR STOP 1
      endif

      if (ANY(ta%a .NE. taa%a) .OR. ANY(ta%b .NE. taa%b) .OR. &
        & ANY(ta%c .NE. taa%c) .OR. ANY(ta%i .NE. taa%i)) then
        ERROR STOP 2
      endif

      if (ANY(tb%a .NE. tba%a) .OR. ANY(tb%b .NE. tba%b) .OR. &
        & ANY(tb%c .NE. tba%c) .OR. ANY(tb%i .NE. tba%i)) then
        ERROR STOP 3
      endif

      end

