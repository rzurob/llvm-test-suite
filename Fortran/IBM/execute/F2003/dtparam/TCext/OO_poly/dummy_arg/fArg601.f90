! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg601.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (use of select type for
!                               unlimited poly dummy-arg)
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

program fArg601
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type (base(4)) :: b1 = base(4) (1)
    class (base(4)), pointer :: b2(:)

    integer(4) :: i

    i = 10

    allocate (b2(0:3), source=(/(child(4,1,15)(ii, 'xlftest 101'), ii=0,3)/))

    call abc (i)
    call abc (b1)

    call abc (b2(2))

    contains

    subroutine abc (x)
        class(*), intent(in) :: x

        !! select type construct should be used here

        select type (x)
            class is (base(4))
                write (*, '(i5)', advance='no') x%id

                select type (x)
                    type is (base(4))

                    type is (child(4,1,*))
                        write (*, '(a,a)', advance='no') ', ', x%name
                    class default
                        error stop 1_4
                end select

                print *, ''
            type is (integer(4))
                print *, x
            class default
                print *, 'other type'
        end select
    end subroutine
end
