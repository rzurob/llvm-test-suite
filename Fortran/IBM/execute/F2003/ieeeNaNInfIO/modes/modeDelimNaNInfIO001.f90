!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeDelimNaNInfIO001.f
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
!*  Testing the input and output of NaN and Inf with various DELIM modes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12

      real(4) :: nanq_pos, nanq_neg, nans_pos, nans_neg, inf_pos, inf_neg

      integer(4)   :: nanq_pos_eq, nanq_neg_eq, nans_pos_eq, nans_neg_eq

      character(3) :: xlf, ibm

      character(7) :: fortran

      character(10) :: list_dir_delim = 'quote', nml_delim='apostrophe'

      integer(4) :: count

      equivalence(nanq_pos, nanq_pos_eq)
      equivalence(nanq_neg, nanq_neg_eq)
      equivalence(nans_pos, nans_pos_eq)
      equivalence(nans_neg, nans_neg_eq)

      namelist /mynml/ nanq_pos, xlf, nanq_neg, ibm, nans_pos,         &
     &                 nans_neg, fortran, inf_pos, inf_neg

      open(in,  file='modeDelimNaNInfIO001.dat', action='read')
      open(out, file='modeDelimNaNInfIO001.out', action='write')

      do count=1, 10, 2

         ! reset variables
         nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
         inf_pos  = 0.0; inf_neg  = 0.0
         xlf = 'xxx'
         ibm = 'xxx'
         fortran = 'xxxxxxx'

         ! read the namelist line of input
         open(in, delim='quote')
         read(in, mynml)

         ! check for the sign of NaN values
         if ( ( nanq_pos_eq .le. 0 ) .or. ( nans_pos_eq .le. 0 ) .or.  &
     &        ( nanq_neg_eq .ge. 0 ) .or. ( nans_neg_eq .ge. 0 ) )     &
     &        call zzrc(count)

         ! write in list-directed
         write(out, *, delim=list_dir_delim)                           &
     &        nanq_pos, xlf, nanq_neg, ibm, nans_pos, nans_neg,        &
     &        fortran, inf_pos, inf_neg

         ! reset variables
         nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
         inf_pos  = 0.0; inf_neg  = 0.0
         xlf = 'xxx'
         ibm = 'xxx'
         fortran = 'xxxxxxx'

         ! read the list-directed line of input
         open(in, delim='apostrophe')
         read(in, *)                                                   &
     &        nanq_pos, xlf, nanq_neg, ibm, nans_pos, nans_neg,        &
     &        fortran, inf_pos, inf_neg

         ! check for the sign of NaN values
         if ( ( nanq_pos_eq .le. 0 ) .or. ( nans_pos_eq .le. 0 ) .or.  &
     &        ( nanq_neg_eq .ge. 0 ) .or. ( nans_neg_eq .ge. 0 ) )     &
     &        call zzrc(count+1_4)


         ! write in namelist
         write(out, nml=mynml, delim=nml_delim)

         ! swap the delim modes for the next namelist an list-directed IO
         call my_swap_delim(list_dir_delim, nml_delim)

      end do

      close(in)
      close(out)

      contains
      subroutine my_swap_delim(str1, str2)
        character(10) :: str1, str2, tmp
        tmp = str1
        str1 = str2
        str2 = tmp
      end subroutine

      end
