! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/class/fclass011a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly-allocatable
!                               components of derived type in intrinsic
!                               assignments)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data(:,:)
    end type

    contains

    subroutine printBaseArray (b)
        class (base(4,*)), intent(in) :: b(:)

        do i = 1, size(b)
            print *, 'element', i

            if (allocated (b(i)%data)) then
                print *, "shape", shape(b(i)%data)

                select type (x => b(i)%data)
                    type is (character(*))
                        print *, x
                    type is (logical(8))
                        print *, x

                    type is (logical(4))
                        print *, x
                end select
            end if
        end do
    end subroutine
end module

program fclass011a1
use m
    type (base(4,20)) b1(3)
    type (base(4,20)), allocatable :: b2(:)

    allocate (b2(0:2))

    allocate (b1(1)%data(2,1), source=reshape((/'test 1', 'test 2'/), (/2,1/)))

    allocate (b1(2)%data(2,2), source=.true._4)

    allocate (b1(3)%data(2,2), source=reshape((/.false._8, .true._8, .true._8, &
                            .false._8/), (/2,2/)))

    !! intrinsic assignment
    b2 = b1

    !! verify results
    call printBaseArray (b2)
end