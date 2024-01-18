!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!*  pointer is dummy argument of external subroutine
!*  pointer is used in forall statement
!*  pointer is redefined by assigning values in the same row to element in same column
!*	   in forall statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        module m
                type parent
                    integer :: id = 8
                end type
                type, extends(parent) :: child
                end type

                interface
                    subroutine sub(ptr)
                        import parent
                        type(parent), pointer :: ptr(:,:)
                    end subroutine
                end interface
        end module

        program main
                use m
                class(parent), allocatable, target :: tar(:)
                type(parent), pointer :: ptr(:,:)

                allocate(child :: tar(20))

                if ( .not. allocated(tar)) stop 17

                select type(tar)
                    type is (child)
                        tar(20:1:-1) = (/(child(i),i=1,20) /)
                    class default
                        stop 21
                end select

                ptr(2:4,-2:0) => tar(:19)

                call sub(ptr)

                if ( any(lbound(ptr) .ne. (/1,1/)) ) stop 31
                if ( any(ubound(ptr) .ne. (/3,3/)) ) stop 33

                print *, ptr%id
        End program

        subroutine sub(ptr)
                use m , only : parent
                type(parent), pointer :: ptr(:,:)

                if ( .not. associated(ptr)) stop 37
                if ( any(lbound(ptr) .ne. (/2,-2/)) ) stop 31
                if ( any(ubound(ptr) .ne. (/4,0/)) ) stop 33

                ptr(1:,1:) => ptr

                forall (i=1:3)
                    ptr(:,i) = ptr(i,:)
                end forall

        end subroutine
