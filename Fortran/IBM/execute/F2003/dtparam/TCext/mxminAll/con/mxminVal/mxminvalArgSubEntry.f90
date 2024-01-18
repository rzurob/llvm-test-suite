! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mxminAll/con/mxminVal/mxminvalArgSubEntry.f
! opt variations: -qck -qnok

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
!*  DESCRIPTION                : MAXVAL/MINVAL with named constant(derived
!*                               type) as actual argument to subprogram 
!*                               with entry statement. No DIM or MASK
!*                               specified.
!* ===================================================================

  program mxminvalArgSubEntry 
    
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

    type dt1(k1,n1)    ! (4,3)
       integer, kind :: k1
       integer, len  :: n1
       character(n1)    x1(3)
    end type 

    type dt2(k2,n2)    ! (4,3)
       integer, kind :: k2
       integer, len  :: n2
       character(n2)    x2(3)
    end type 

    character*2, x(5,6,7,3), y(5,6,7,3), z
    character*3  v

    type(dt1(4,3)) n1_dt
    type(dt2(4,3)) n2_dt

    parameter(x = "\_m", y = "\nm")

    parameter(n1_dt = dt1(4,3)("123"))
    parameter(n2_dt = dt2(4,3)("456"))

    call sub(z, maxval(x), minval(y))

    if(z .ne. "\nm") error stop 1_4

    call ent(z, minval(x), maxval(y))

    if(z .ne. "\_m") then
         error stop 2_4
    endif 

    call sub1(v, minval(n1_dt%x1), maxval(n2_dt%x2))

    if(v .ne. "123") error stop 3_4

  end program mxminvalArgSubEntry 

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


