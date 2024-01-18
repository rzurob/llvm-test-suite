!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with optional argument
!*                               
!*
!* ===================================================================

  program mxminArrayArgSubAttr1 

    interface
       subroutine sub1(c1arg, c2arg)
           character*3, dimension(2,4) :: c1arg, c2arg
           optional c2arg
       end subroutine
       subroutine sub2(c1arg, c2arg)
           character*3, dimension(2,4) :: c1arg, c2arg, c3arg
           optional c2arg
       end subroutine
    end interface

    character*3 x(2,4), y
    parameter(x = "ddd", y = "bbb")

    call sub1(max(x, y))

    call sub2(max(x, y), min(x, y))

  end program mxminArrayArgSubAttr1 

  subroutine sub1(c1arg, c2arg)
      character*3, dimension(2,4) :: c1arg, c2arg, c3arg
      optional c2arg

      if(present(c2arg) .neqv. .false.) then
          error stop 1_4
      endif

      c3arg = max(c1arg, c1arg, c2arg) 

  end subroutine


  subroutine sub2(c1arg, c2arg)
      character*3, dimension(2,4) :: c1arg, c2arg, c3arg
      optional c2arg

      if(present(c2arg) .neqv. .true.) then
          error stop 2_4
      endif

      c3arg = "#_%"

      if(any(max(c1arg, c2arg, c2arg) .ne. "ddd")) then
           error stop 3_4
      endif

  end subroutine

