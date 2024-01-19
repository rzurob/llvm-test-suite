!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable as actual
!*                               argument to subprogram
!* ===================================================================

  module subArgVar
     contains
        subroutine sub2(argout, argin)
           integer, intent(in):: argin(2)
           integer, intent(out) :: argout(2)
           argout = argin
        end subroutine
  end module subArgVar

  program mxminLocVarArrArgSub1

    use subArgVar

    interface
        function func1(carg, n)
          integer, dimension(*) :: carg
          integer, dimension(n) :: func1
        end function
    end interface

    integer :: v(2) = 0
    integer :: v1
    logical :: m(2,2) = .true.
    character*3 :: x(5) = (/(char(i+70), i = 1,10,2)/)
    character*3 :: y(2,2) = reshape((/"bbb", "aaa", "ccc", "ddd"/),(/2,2/))

    m(1,2) = .false.

    call sub2(v, minloc(y))

    if(v(1) .ne. 2 .or. v(2) .ne. 1) error stop 1_4

    call sub2(v, maxloc(y, mask=m))

    if(v(1) .ne. 2 .or. v(2) .ne. 2) error stop 2_4

    call sub2(v, maxloc(y, dim=2))

    if(v(1) .ne. 2 .or. v(2) .ne. 2) error stop 3_4

    call sub2(v, maxloc(y, dim=1, mask=m))

    if(v(1) .ne. 1 .or. v(2) .ne. 2) error stop 4_4

    call sub1(v, maxloc(y))

    if(v(1) .ne. 2 .or. v(2) .ne. 2) error stop 5_4

    call sub1(v, maxloc(y, dim=1, mask=m))

    if(v(1) .ne. 1 .or. v(2) .ne. 2) error stop 6_4

    call sub3(v1, maxloc(x, dim=1))

    if(v1 .ne. 5) error stop 7_4

    v = func1(minloc(y), 2)

    if(v(1) .ne. 2 .or. v(2) .ne. 1) error stop 8_4

    v = func1(maxloc(y, dim=1, mask=m), 2)

    if(v(1) .ne. 1 .or. v(2) .ne. 2) error stop 9_4

    v = func1(maxloc(y, dim=2), 2)

    if(v(1) .ne. 2 .or. v(2) .ne. 2) error stop 10_4

    contains
         subroutine sub1(argout, argin)
            integer, dimension(2) :: argout, argin
            argout = argin
         end subroutine

         subroutine sub3(argout, argin)
            integer :: argout, argin
            argout = argin
         end subroutine

  end program mxminLocVarArrArgSub1

  function func1(carg, n)
       integer, dimension(*) :: carg
       integer, dimension(n) :: func1
       do i = 1, n
           func1(i) = carg(i)
       end do
  end function

