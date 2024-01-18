!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO013.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 26, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing to make sure when doing integer input, different forms of
!*  NaN and Inf are not read as integer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
      implicit none

      integer(4)  :: i1, i2, count
      integer(8)  :: i8
      
      integer, parameter :: in = 11
      
      open(unit=in, file='miscNaNInfIO013.dat', action='read')
      
      do count = 1, 2
         
         i1 = -1_4
         i8 = -1_8
         i2 = -1_4
         
         read(in, *) i1, i8, i2

         if ( (i1 .ne. 0_4) .or. (i8 .ne. 0_8) .or. (i2 .ne. 0_4) ) then
            print *, "list-directed input failed"
            print *, i1, i8, i2
            call zzrc(count)
         end if

         i1 = -1_4
         i8 = -1_8
         i2 = -1_4

         read(in, '(3I10.2)') i1, i8, i2
         
         if ( (i1 .ne. 0_4) .or. (i8 .ne. 0_8) .or. (i2 .ne. 0_4) ) then
            print *, "format-directed input failed"
            print *, i1, i8, i2
            call zzrc(count)
         end if         
         

      end do
      
      
      end
