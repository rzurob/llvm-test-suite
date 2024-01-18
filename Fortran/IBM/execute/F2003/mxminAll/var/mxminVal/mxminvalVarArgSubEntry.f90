!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with variable(derived
!*                               type) as actual argument to subprogram 
!*                               with entry statement.
!* ===================================================================

  program mxminvalVarArgSubEntry 
    
    interface
       elemental subroutine sub(res, arg1, arg2)
         character*2, intent(in) :: arg1
         character*2, intent(in) :: arg2
         character*2, intent(out):: res
       end subroutine sub
       elemental subroutine ent(res, arg1, arg2)
         character*2, intent(in) :: arg1
         character*2, intent(in) :: arg2
         character*2, intent(out):: res
       end subroutine ent 
       elemental subroutine sub1(res, arg1, arg2)
         character*3, intent(in) :: arg1
         character*3, intent(in) :: arg2
         character*3, intent(out):: res
       end subroutine sub1
    end interface

    type dt1
       character*3 x1(3)
    end type 

    type dt2
       character*3 x2(3)
    end type 

    character*2, x(5,6,7,3), y(5,6,7,3), z
    character*3  v

    type(dt1) n1_dt
    type(dt2) n2_dt

    x = "\_m"
    y = "\nm"

    n1_dt = dt1("123")
    n2_dt = dt2("456")

    call sub(z, maxval(x), minval(y))

    if(z .ne. "\nm") error stop 1_4

    call ent(z, minval(x), maxval(y))

    if(z .ne. "\_m")  error stop 2_4

    call sub1(v, minval(n1_dt%x1), maxval(n2_dt%x2))

    if(v .ne. "123") error stop 3_4

  end program mxminvalVarArgSubEntry

  elemental subroutine sub(res, arg1, arg2)
         character*2, intent(in) :: arg1
         character*2, intent(in) :: arg2
         character*2, intent(out):: res
         res = min(arg1 , arg2)
         return
   entry ent (res, arg1, arg2)
         res = max(arg2, arg1)
         return
   end subroutine sub

  elemental subroutine sub1(res, arg1, arg2)
         character*3, intent(in) :: arg1
         character*3, intent(in) :: arg2
         character*3, intent(out):: res
         res = min(arg1 , arg2)
  end subroutine sub1


