! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fiomsg.presh fiomsg12
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For IOMSG= variable in I/O statements.
!*                             :
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : IOMSG variable in I/O list. It must 
!*                               be assigned the error msg if an error
!*                               occurs.
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        Program fiomsg12
          character(245) iomsg_var
          integer i
          open(11, status='new', access='stream', form='formatted', iostat=i)
          write(11, fmt='($, A10)', iostat=iostat) '1234567890'
          rewind (11, iomsg=iomsg_var)
          read(11, fmt='(A10)', iostat=iostat, iomsg=iomsg_var) iomsg_var
          print*, iomsg_var
          read(11, fmt='(A10)', iostat=iostat, iomsg=iomsg_var) iomsg_var
          print*, iomsg_var
        end program fiomsg12
