!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : extNaNInfIO005.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 17, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=oldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing the non-standard forms of NaN on input/output supported 
!*  by extension using internal I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program extNaNInfIO005

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(40) :: buff_in
      character(53) :: buff_out
      character(3) :: cc
      complex(4)   :: cx
      real(4)      :: nanq_pos, nans_pos, nanq_neg, nans_neg, inf_neg, inf_pos

      character(53), parameter :: verif_str =                         &
     &     ' -INF +NaNQ +NaNS +INF "inf" -NaNS (+NaNS,+INF) -NaNQ'


      call setrteopts("langlvl=extended")

      buff_in = 'NaNQ -nanQ (+nanS; +INF); inf -NANS -inf'

      ! read in the values and initialize items inside the namelist
      read(buff_in, *, decimal='comma')                                &
     &     nanq_pos, nanq_neg, cx, cc, nans_neg, inf_neg

     
      ! check the values just read
     
      nans_pos  = real(cx)
      inf_pos   = imag(cx)
      
      if( ieee_is_finite(inf_pos) .or. ieee_is_negative(inf_pos) )     &
     &     error stop 1_4
      
      if( ( .not. ieee_is_nan(nanq_neg) ) .or.                         &
     &    ( ieee_class(nanq_neg) .ne. ieee_quiet_nan ) .or.            &
     &    ( .not. equiv_is_negative(nanq_neg) ) ) error stop 2_4

      if( ( .not. ieee_is_nan(nans_pos) ) .or.                         &
     &    (ieee_class(nans_pos) .ne. ieee_signaling_nan ) .or.         &
     &    ( .not. equiv_is_positive(nans_pos) ) ) error stop 3_4

      if( ( .not. ieee_is_nan(nanq_pos) ) .or.                         &
     &    ( ieee_class(nanq_pos) .ne. ieee_quiet_nan ) .or.            &
     &    ( .not. equiv_is_positive(nanq_pos) ) ) error stop 4_4
      
      if(ieee_is_finite(inf_neg) .or. .not. ieee_is_negative(inf_neg)) &
     &     error stop 5_4      

      if( ( .not. ieee_is_nan(nans_neg) ) .or.                         &
     &    (ieee_class(nans_neg) .ne. ieee_signaling_nan) .or.          &
     &    ( .not. equiv_is_negative(nans_neg) ) ) error stop 6_4

      if ( cc .ne. 'inf' ) error stop 7_4
      
      
      ! write out the namelist
      write(buff_out, *, delim='quote', sign='plus')                   &
     &  inf_neg, nanq_pos, nans_pos, inf_pos, cc, nans_neg, cx, nanq_neg

      ! verify internal output:
      if ( buff_out .ne. verif_str ) error stop 8_4
     

      contains

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is negative
      logical function equiv_is_negative(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq
         
         equivalence(tmp_val, val_eq)
         
         tmp_val = val
         
         if ( val_eq .ge. 0 ) then
            equiv_is_negative = .false.
         else
            equiv_is_negative = .true.
         end if

      end function

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is positive
      logical function equiv_is_positive(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq
         
         equivalence(tmp_val, val_eq)
         
         tmp_val = val

         if ( val_eq .le. 0 ) then
            equiv_is_positive = .false.
         else
            equiv_is_positive = .true.
         end if

      end function

end program
