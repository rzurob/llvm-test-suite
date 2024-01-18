! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is nonpoly DT of child  with len&kind param
!*                               TO is poly DT of base with len param
!*                               Pnt is of type class(*), func name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(l)
       integer, len :: l
       character(l), allocatable :: ch(:)
   end type

   type, extends(base) :: child (k)
       integer, kind :: k
       integer(k*2), allocatable :: id(:)
   end type

   contains
       function func(arg, brg)
           type( child(l=3, k=2)), allocatable :: arg
           class( base(3)), allocatable :: brg
           target arg, brg
           class(*), pointer :: func

           func => arg
           call move_alloc(arg, brg)

           if ( .not. associated( func, brg) ) error stop 21
      end function
end module

use m
   type(child(l=3,k=2)),  allocatable :: a1
   class(base(3)), allocatable :: a2


   allocate(a1, source = child(3,2)( (/'xyz', 'ibm'/),  (/1,2,3,4,5/) ) )

   select type ( x => func(a1, a2) )
       type is (child(*,2))
           print *, x%ch
           print *, x%id
    end select

end
