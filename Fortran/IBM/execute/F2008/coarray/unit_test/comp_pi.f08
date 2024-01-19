!*  ===================================================================
!*
!*  DATE                       : July 31, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray access
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of coarray
!*                               access. This test case was
!*                               originally from Jim Xia and was
!*                               converted from MPI.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program comp_pi
    implicit none
    real(8), parameter :: PICONST = 3.1415926535897932d0

    real(8), save :: mypi[*], error[*]
    real(8) pi,w,x
    integer, save :: n[*]
    integer numprocs, me, i, j
    integer, parameter :: MAXN = 1024*1024

    numprocs = num_images()
    me = this_image()

    n = 2*numprocs
    if (n < 16) n = 16/numprocs*numprocs

2   if (me == 1) then
        n = n * 2
    end if
    sync all

    n = n[1]
    w = 1.0d0 / n
    mypi = 0.0
    do i = 1, n/numprocs
        x = (me-1.0d0)/numprocs + (i-0.5d0)*w
        mypi = mypi + f(1.0d0*x) * w
    end do
    sync all

    if (me == 1) then
        pi = 0
        do i = 1, numprocs
            pi = pi + mypi[i]
        end do
        error = abs(pi/PICONST-1.0d0)
    end if
    sync all

    error = error[1]
    if (error > 1.0d-12) then ! we require 12 significant digits
        if (n < MAXN) then
            goto 2
        else ! in error
            print *, 'image', me, 'failed to converge. Error = ', error
            error stop 1
        end if
    end if
    sync all  !<-- this should be removed once we support exit behaviour

    contains
    pure double precision function f(x)
        real(8), intent(in) :: x
        f = 4.0d0 / (1.d0 + x*x)
    end function
    end
