! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/ptrAssc/polyPntFmUlmtTo.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is component of poly type base
!*                               TO is of class(*), component of DT
!*                               pointer is of poly type base
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
       class(*), allocatable :: l1
   end type

   type, extends(base) :: child    ! (4,20)
       class(base(k1,:)), allocatable :: l2
    end type

end module

use m

    class(child(4,:)), target,  allocatable :: C

    class(base(4,:)), pointer :: p

    type(base(4,20))  B

    B = base(4,20) ( 'ABC-XYZ' )

    allocate(C, source = child(4,20)(l1 =' 123456' ,l2 = B) )

    p => C%l2

    call move_alloc( C%l2, C%l1 )

    select type ( x=> C%l1)
        type is ( base(4,*))
            if ( .not. associated(p, x) ) error stop 23
            select type ( y  => x%l1 )
                type is (character(*))
                    print *, y
            end select
    end select

end
