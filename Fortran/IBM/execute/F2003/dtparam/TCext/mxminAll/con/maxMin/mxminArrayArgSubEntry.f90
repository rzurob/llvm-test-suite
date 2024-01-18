! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mxminAll/con/maxMin/mxminArrayArgSubEntry.f
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
!*  DESCRIPTION                : MAX/MIN with named constant(also with
!*                               derived type) as actual argument to
!*                               subprogram with entry statement
!*
!*                               elemental subprogram
!* ===================================================================

  program mxminArrayArgSubEntry 
    
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

    character*2, x(5,6,7,3), y(5,6,7,3), z(5,6,7,3)
    character*3  v(3)

    type(dt1(4,3)) n1_dt
    type(dt2(4,3)) n2_dt

    parameter(x = "\_m", y = "\nm")

    parameter(n1_dt = dt1(4,3)("123"))
    parameter(n2_dt = dt2(4,3)("456"))

    call sub(z, max(x, y, min(x, y)), min(x,y, max(x, y)))

    if(any(z .ne. "\nm")) then
        error stop 1_4
    endif

    call ent(z, max(x, y, min(x, y)), min(x,y, max(x, y)))

    if(any(z .ne. "\_m")) then
         error stop 2_4
    endif 

    call sub1(v, min(n1_dt%x1, n2_dt%x2), max(n1_dt%x1, n2_dt%x2))

    if(any(v .ne. "123")) then
          error stop 3_4
    endif

  end program mxminArrayArgSubEntry 

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


