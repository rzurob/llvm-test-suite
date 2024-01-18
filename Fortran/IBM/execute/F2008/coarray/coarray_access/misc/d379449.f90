! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-15
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 379449
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

program d379449
    integer, save :: i1(1000)[*]

    integer, allocatable :: x(:)
    integer np, i, j, k
    interface
        subroutine verify(j,n)
            integer, intent(in) :: j, n
        end subroutine
    end interface
    np = num_images()

    if (this_image() == 1) then

        allocate(x(np*size(i1)))

        do i = 1, np*size(i1)
            x(i) = i
        end do

        k = 1
        do i = 1, np
            do j = 1, size(i1)
                i1(j)[i] = x(k)
                k = k + 1
            end do
        end do
    end if
    sync all
    do i = 1, size(i1)
        call verify (i1(i), size(i1)*(this_image()-1)+i)
        call verify_implicit (i1(i), size(i1)*(this_image()-1)+i)
    end do

    sync all
    end

    subroutine verify (j, n)
        implicit none
        integer, intent(in) :: j, n

        if (j /= n) then
            print *, 'failed in verify.', this_image(), ':', j, n
            error stop 1
        end if
    end subroutine

    subroutine verify_implicit (j, n)
        implicit none
        integer, intent(in) :: j, n

        if (j /= n) then
            print *, 'failed in verify_implicit.',this_image(), ':', j, n
            error stop 1
        end if
    end subroutine

