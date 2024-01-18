!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : modePadNaNInfIO001.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 4, 2006
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
!*  Testing the input and output of NaN and Inf with various PAD modes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12
      
      real(4) :: nanq_pos, nanq_neg, nans_pos, nans_neg, inf_pos, inf_neg

      integer(4)   :: nanq_pos_eq, nanq_neg_eq, nans_pos_eq, nans_neg_eq

      character(3) :: xlf, ibm

      integer(4) :: count

      equivalence(nanq_pos, nanq_pos_eq)
      equivalence(nanq_neg, nanq_neg_eq)
      equivalence(nans_pos, nans_pos_eq)      
      equivalence(nans_neg, nans_neg_eq)


      open(in,  file='modePadNaNInfIO001.dat', action='read')
      open(out, file='modePadNaNInfIO001.out', action='write', pad='no')

      do count=1, 5

         ! reset variables
         nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
         inf_pos  = 0.0; inf_neg  = 0.0
         xlf = 'xxx'
         ibm = 'xxx'

         read(in, '(f9.1,a3,f9.2,a3,4f9.1)', pad='no')                 &
     &        nanq_pos, xlf, nanq_neg, ibm, nans_pos, nans_neg,        &
     &        inf_pos, inf_neg

         ! check for the sign of NaN values
         if ( ( nanq_pos_eq .le. 0 ) .or. ( nans_pos_eq .le. 0 ) .or.  &
     &        ( nanq_neg_eq .ge. 0 ) .or. ( nans_neg_eq .ge. 0 ) )     &
     &        call zzrc(count)
         
         write(out, *)  nanq_pos, xlf, nanq_neg, ibm, nans_pos,        &
     &                  nans_neg, inf_pos, inf_neg

      end do
    
      close(in)
      close(out)

      
      end
