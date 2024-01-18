!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 4, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing the input and output of NaN and Inf with various modes.
!*  Change modes using OPEN statements.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12

      real(4) :: nanq_pos, nanq_neg, nans_pos, nans_neg, inf_pos, inf_neg

      integer(4)   :: nanq_pos_eq, nanq_neg_eq, nans_pos_eq, nans_neg_eq

      character(3) :: xlf, ibm

      equivalence(nanq_pos, nanq_pos_eq)
      equivalence(nanq_neg, nanq_neg_eq)
      equivalence(nans_pos, nans_pos_eq)
      equivalence(nans_neg, nans_neg_eq)

      open(in,  file='modeMixNaNInfIO001.dat', action='read')
      open(out, file='modeMixNaNInfIO001.out', action='write')

      !************************************************************
      !** FIRST SET OF VALUES
      !************************************************************

      ! reset variables
      nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
      inf_pos  = 0.0; inf_neg  = 0.0
      xlf = 'xxx'
      ibm = 'xxx'

      open(in, delim='quote', blank='zero', sign='plus')
      read(in, *) nanq_pos, xlf, nanq_neg, ibm, nans_pos, nans_neg,    &
     &            inf_pos, inf_neg

     ! check for the sign of NaN values
      if ( ( nanq_pos_eq .le. 0 ) .or. ( nans_pos_eq .le. 0 ) .or.     &
     &     ( nanq_neg_eq .ge. 0 ) .or. ( nans_neg_eq .ge. 0 ) )        &
     &     error stop 1_4

      open(out, delim='quote', blank='zero', sign='plus')
      write(out, *) nanq_pos, xlf, nanq_neg, ibm, nans_pos,            &
     &              nans_neg, inf_pos, inf_neg

      !************************************************************
      !** SECOND SET OF VALUES
      !************************************************************

      ! reset variables
      nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
      inf_pos  = 0.0; inf_neg  = 0.0
      xlf = 'xxx'
      ibm = 'xxx'

      open(in, delim='none', blank='null', sign='suppress')
      read(in, *) nanq_pos, xlf, nanq_neg, ibm, nans_pos, nans_neg,    &
     &            inf_pos, inf_neg

     ! check for the sign of NaN values
      if ( ( nanq_pos_eq .le. 0 ) .or. ( nans_pos_eq .le. 0 ) .or.     &
     &     ( nanq_neg_eq .ge. 0 ) .or. ( nans_neg_eq .ge. 0 ) )        &
     &     error stop 2_4

      open(out, delim='apostrophe', blank='null', sign='suppress')
      write(out, *) nanq_pos, xlf, nanq_neg, ibm, nans_pos,            &
     &              nans_neg, inf_pos, inf_neg

      !************************************************************
      !** THIRD SET OF VALUES
      !************************************************************

      ! reset variables
      nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
      inf_pos  = 0.0; inf_neg  = 0.0
      xlf = 'xxx'
      ibm = 'xxx'

      open(in, delim='apostrophe', blank='null', sign='plus')
      read(in, *) nanq_pos, xlf, nanq_neg, ibm, nans_pos, nans_neg,    &
     &            inf_pos, inf_neg

     ! check for the sign of NaN values
      if ( ( nanq_pos_eq .le. 0 ) .or. ( nans_pos_eq .le. 0 ) .or.     &
     &     ( nanq_neg_eq .ge. 0 ) .or. ( nans_neg_eq .ge. 0 ) )        &
     &     error stop 3_4

      open(out, delim='apostrophe', blank='null', sign='plus')
      write(out, *) nanq_pos, xlf, nanq_neg, ibm, nans_pos,            &
     &              nans_neg, inf_pos, inf_neg


      close(in)
      close(out)

      end
