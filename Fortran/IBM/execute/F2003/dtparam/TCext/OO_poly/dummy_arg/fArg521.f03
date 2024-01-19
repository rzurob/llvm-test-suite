! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg521.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : array constructor (generic-name in array
!                               constructor)
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

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name ='default'
    end type

    interface makeData
        function makeBase (id, n)
        import base
            integer*4, intent(in) :: id, n
            type (base(4)) :: makeBase(n)
        end function

        function makeChildData (id, name, n)
        import child
            integer*4, intent(in) :: id, n
            character(*), intent(in) :: name

            type (child(4,1,20)) :: makeChildData (n)
        end function
    end interface
end module

program fArg521
use m
    type(base(4)) :: b1 (10)
    type (child(4,1,20)) :: c1 (5)

    b1 = (/makeData(1,3), makeData(10, 6), base(4)(20)/)

    print *, b1

    c1 = (/child(4,1,20)(30), makeData(100, 'temp2-4', 3), child(4,1,20)(2, 'temp5')/)

    print *, c1
end

type (base(4)) function makeBase (id, n)
use m, only: base
    integer*4, intent(in) :: id, n
    dimension makeBase(n)

    makeBase%id = id
end function

type (child(4,1,20)) function makeChildData (id, name, n)
use m, only:child
    integer*4, intent(in) :: id, n
    character(*), intent(in) :: name
    dimension makeChildData (n)

    makeChildData%id = id
    makeChildData%name = name
end function
