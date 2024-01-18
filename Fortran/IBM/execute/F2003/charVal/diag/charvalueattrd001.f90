!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrd001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : charvalueattrd001
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Jan. 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for character dummy
!*                               arguments with length other than 1 to  
!*                               have the VALUE attribute (Feature 298120).
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*  REQUIRED RUNTIME OPTIONS   : 
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if characters of length 1 or other than 1  
!*                               have the VALUE attribute, they don't get
!*                               flagged at compile-time, as long as the
!*                               length is known at compile-time.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      interface gen1
         subroutine ss1(a)
           character(1), value :: a
         end subroutine
      end interface
      interface gen2
         subroutine ss2(b)
           character(len=2), value :: b
         end subroutine
      end interface
      interface gen3
         subroutine ss3(c)
           character*111, value :: c
         end subroutine
      end interface
      interface gen4
         subroutine ss4(d)
           character(0), value :: d
         end subroutine
      end interface

      interface gen5
         subroutine s1(a)
           character(len=1) a
           value :: a
         end subroutine
      end interface
      interface gen6
         subroutine s2(b)
           character*2 :: b
           value :: b
         end subroutine
      end interface
      interface gen7
         subroutine s3(c)
           character(111) :: c
           value :: c
         end subroutine
      end interface
      interface gen8
         subroutine s4(d)
           character(0) :: d
           value :: d
         end subroutine
      end interface

      interface gen9
         subroutine sss1(a)
           value :: a
           character(1) a
         end subroutine
      end interface
      interface gen10
         subroutine sss2(b)
           value :: b
           character(2) :: b
         end subroutine
      end interface
      interface gen11
         subroutine sss3(c)
           value :: c
           character(111) :: c
         end subroutine
      end interface
      interface gen12
         subroutine sss4(d)
           value :: d
           character(0) :: d
         end subroutine
      end interface
      
      end


      subroutine s1(a)
       character(1), value :: a
      end subroutine
      subroutine s2(b)
       character(2), value :: b
      end subroutine
      subroutine s3(c)
       character(111), value :: c
      end subroutine
      subroutine s4(d)
       character(0), value :: d
      end subroutine


      subroutine ss1(a)
       character(1) :: a
       value :: a
      end subroutine
      subroutine ss2(b)
       character(2) :: b
       value :: b
      end subroutine
      subroutine ss3(c)
       character(111) :: c
       value :: c
      end subroutine
      subroutine ss4(d)
       character(0) :: d
       value :: d
      end subroutine


      subroutine sss1(a)
       value :: a
       character(1) :: a
      end subroutine
      subroutine sss2(b)
       value :: b
       character(2) :: b
      end subroutine
      subroutine sss3(c)
       value :: c
       character(111) :: c
      end subroutine
      subroutine sss4(d)
       value :: d
       character(0) :: d
      end subroutine
