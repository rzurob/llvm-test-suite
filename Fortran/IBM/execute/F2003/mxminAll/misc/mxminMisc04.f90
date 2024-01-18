!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL and MAXLOC/MINLOC as actual
!*                               argument passed to subprogram with variable
!*                               argument.
!*
!* ===================================================================

module misc4

    character*3, dimension(2,3):: x

    data ((x(i, j), i=1, 2), j = 1,3) /6*'gta'/

    contains

          elemental function fun1(arg)
             character*3, intent(in):: arg
             character*3 fun1
             fun1 = arg
          end function

          elemental function fun2(arg)
             integer, intent(in)::arg
             integer fun2
             fun2 = arg
          end function

          elemental function fun3(arg)
             integer, intent(in)::arg
             integer fun3
             fun3 = arg
          end function

end module misc4

program mxminMisc04
    use misc4

    character*3 ver(2)
    integer     ver_int(2) , ver2(2), ver3(3)

    x(2,1) = 'zdf'

    ver = fun1(maxval(x, dim=2, mask = .true.))

    if(ver(1) .ne. 'gta' .or. ver(2) .ne. 'zdf') then
         error stop 1_4
    endif

    if (fun1(maxval(x, mask = .true.)) .ne. 'zdf') then
          error stop 2_4
    endif

    if(fun1(maxval(x)) .ne. 'zdf') then
        error stop 3_4
    endif

    ver_int = fun2(maxloc(x))

    if(ver_int(1) .ne. 2 .or. ver_int(2) .ne. 1) then
          error stop 4_4
    endif

    ver_int = fun2(minloc(x, mask = .true.))

    if(ver_int(1) .ne. 1 .or. ver_int(2) .ne. 1) then
           error stop 5_4
    endif

    ver3 = fun3(maxloc(x, dim =1,  mask = .true.))
    ver2 = fun3(minloc(x, dim =2,  mask = .true.))

    if(ver3(1) .ne. 2 .or. ver3(2) .ne. 1 .or. ver3(3) .ne. 1) then
        error stop 6_4
    endif

    if(ver2(1) .ne. 1 .or. ver2(2) .ne. 2) then
         error stop 7_4
    endif

end program mxminMisc04

