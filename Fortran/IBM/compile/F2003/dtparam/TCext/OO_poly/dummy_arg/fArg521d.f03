! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg521d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : array constructor (all elements in the array
!                               constructor shall have the same declared type)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface makeData
        function makeBaseObj (id, n)
        import base
            integer*4, intent(in) :: id, n
            type (base(4)) :: makeBaseObj (n)
        end function

        function makeChildObj (id, name, n)
        import child
            integer*4, intent(in) :: id, n
            character(*), intent(in) :: name
            type (child(4,1,20)) :: makeChildObj (n)
        end function
    end interface
end module

program fArg521d
use m
    print *, (/makeData (1, 3), makeData (2, 'childData', 2)/)
end