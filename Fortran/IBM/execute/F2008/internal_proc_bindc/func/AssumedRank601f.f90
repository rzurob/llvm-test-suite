! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank601f.f
!*
!* ORIGINAL PROGRAMMER          : Dorra Bouchiha
!* PROGRAMMER                   : Izhak Jakov
!* DATE                : September 9, 2015
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    : 
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in Fortran from C
!*                                - array is rank 1
!*                                - CFI_attribute_other: Lower bound is always 1 on the Fortran side 
!*                                - type c_int
!*                                - nested with bind(c)=>bind(c) call
!*
!*
!* Actual Argument:
!*
!* Dummy Argument:
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main
  use ISO_C_BINDING

  interface
    subroutine cfun(x) bind(c)
      use ISO_C_BINDING, ONLY : C_FUNPTR
      type(c_funptr), value:: x
    end subroutine cfun
    
    subroutine sub(arr, r, s, lb, ub) bind(c)
        implicit none
        integer :: arr(..), r, s, lb, ub
    end subroutine sub
    subroutine sub_contig(arr) bind(c)
        implicit none
        integer, contiguous :: arr(..)
    end subroutine sub_contig
  end interface
    
  type(c_funptr) :: cproc
  cproc = c_funloc(fcheck)
  call cfun(cproc)
  
  contains
  subroutine fcheck(flag, arr) bind(c)
      integer        :: i 
      integer(c_int) :: flag
      integer(c_int) :: arr(..)
  
  !    interface 
  !        subroutine sub(arr, r, s, lb, ub) bind(c)  
  !            implicit none
  !            integer :: arr(..), r, s, lb, ub
  !        end subroutine sub
  !        subroutine sub_contig(arr) bind(c)
  !            implicit none
  !            integer, contiguous :: arr(..)
  !        end subroutine sub_contig
  !    end interface 
  
  !      /* 
  !       flag set to 0 when lbound is 1
  !       flag set to 1 when lbound is 1
  !       flag set to 2 when lbound is 1
  !       flag set to -1 when array is not contiguous and lbound is 1
  !       */
     
      if ( flag .eq. -1 ) then
          if(           is_contiguous(arr) ) ERROR STOP 11
          if( rank(arr)      .ne.        1 ) ERROR STOP 12
          if( size(arr)      .ne.        4 ) ERROR STOP 13
          if( any(shape(arr) .ne.     [4]) ) ERROR STOP 14
          if( lbound(arr,1)  .ne.        1 ) ERROR STOP 15
          if( ubound(arr,1)  .ne.        4 ) ERROR STOP 16
          call sub(arr, 1, 4, 1, 4)
          call sub_contig(arr)
      elseif ( flag .eq. 0 ) then
          if(     .not. is_contiguous(arr) ) ERROR STOP 21
          if( rank(arr)      .ne.        1 ) ERROR STOP 22
          if( size(arr)      .ne.       10 ) ERROR STOP 23
          if( any(shape(arr) .ne.    [10]) ) ERROR STOP 24
          if( lbound(arr,1)  .ne.        1 ) ERROR STOP 25
          if( ubound(arr,1)  .ne.       10 ) ERROR STOP 26
          call sub(arr, 1, 10, 1, 10)
          call sub_contig(arr)
      elseif ( flag .eq. 1 ) then
          if(     .not. is_contiguous(arr) ) ERROR STOP 31
          if( rank(arr)      .ne.        1 ) ERROR STOP 32
          if( size(arr)      .ne.       10 ) ERROR STOP 33
          if( any(shape(arr) .ne.    [10]) ) ERROR STOP 34
          if( lbound(arr,1)  .ne.        1 ) ERROR STOP 35
          if( ubound(arr,1)  .ne.       10 ) ERROR STOP 36
          call sub(arr, 1, 10, 1, 10)
          call sub_contig(arr)
      elseif ( flag .eq. 2 ) then
          if(     .not. is_contiguous(arr) ) ERROR STOP 41
          if( rank(arr)      .ne.        1 ) ERROR STOP 42
          if( size(arr)      .ne.       10 ) ERROR STOP 43
          if( any(shape(arr) .ne.    [10]) ) ERROR STOP 44
          if( lbound(arr,1)  .ne.        1 ) ERROR STOP 45
          if( ubound(arr,1)  .ne.       10 ) ERROR STOP 46
          call sub(arr, 1, 10, 1, 10)
          call sub_contig(arr)
      else
          if(     .not. is_contiguous(arr) ) ERROR STOP 51
          if( rank(arr)      .ne.        1 ) ERROR STOP 52
          if( size(arr)      .ne.       10 ) ERROR STOP 53
          if( any(shape(arr) .ne.    [10]) ) ERROR STOP 54
          if( lbound(arr,1)  .ne.        0 ) ERROR STOP 55
          if( ubound(arr,1)  .ne.        9 ) ERROR STOP 56
          call sub(arr, 1, 10, 0, 9)
          call sub_contig(arr)
      end if
  end subroutine fcheck

end program main
  
subroutine sub(arr, r, s, lb, ub) bind(c)
    implicit none
    integer :: arr(..), r, s, lb, ub

    if( rank(arr)      .ne.        r ) ERROR STOP 101
    if( size(arr)      .ne.        s ) ERROR STOP 102
    if( any(shape(arr) .ne.     [s]) ) ERROR STOP 103
    if( lbound(arr,1)  .ne.       lb ) ERROR STOP 104
    if( ubound(arr,1)  .ne.       ub ) ERROR STOP 105
end subroutine sub

subroutine sub_contig(arr) bind(c)  
    implicit none
    integer, contiguous :: arr(..)

    if( .not. is_contiguous(arr) ) ERROR STOP 100
end subroutine sub_contig
