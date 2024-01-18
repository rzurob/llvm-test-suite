!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk074b cxcmn_blk074
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk074b fxcmn_blk074b.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : March 19, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95, xlc, gcc 
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array 
!*				 variables inside of common blocks are interoperable 
!*				 with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  INTEGER(C_INT16_T)
!*					
!*                               Test: BIND(C) common block in external subroutine 
!*					
!* ===================================================================
!*  REVISION HISTORY					
!*  MM/DD/YY:  Init:  Comments:			
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk074b
	use iso_c_binding
        implicit none

      call extern_fsub()

End program

subroutine extern_fsub()
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
! ----------------------------------------------------------------------------


	INTEGER (C_INT16_T 		)		:: int_C_INT16_T(5)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement  
! ----------------------------------------------------------------------------

	COMMON     /blk_int_C_INT16_T/      int_C_INT16_T       
	bind(c, Name ='_______________________________________________________________________')  :: /blk_int_C_INT16_T/  

! ----------------------------------------------------------------------------
! Integer Initialization
! ----------------------------------------------------------------------------

	int_C_INT16_T                   = (/o'77777',o'0',-32768, 32767,b'1111111'/)      

! ----------------------------------------------------------------------------
! Integer Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

	if ( int_c_int16_t(1)           .ne.  o'77777'              )    error stop 10
	if ( int_c_int16_t(2)           .ne.  o'0'                  )    error stop 11
	if ( int_c_int16_t(3)           .ne. -32768                 )    error stop 12
	if ( int_c_int16_t(4)           .ne.  32767                 )    error stop 13
	if ( int_c_int16_t(5)           .ne.  b'1111111'            )    error stop 14

! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

	CALL CSUB_INT()


! ----------------------------------------------------------------------------
! Integer Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

	if ( int_c_int16_t(5)           .ne.  o'77777'              )    error stop 20
	if ( int_c_int16_t(4)           .ne.  o'0'                  )    error stop 21
	if ( int_c_int16_t(3)           .ne. -32768                 )    error stop 22
	if ( int_c_int16_t(2)           .ne.  32767                 )    error stop 23
	if ( int_c_int16_t(1)           .ne.  b'1111111'            )    error stop 24

   end subroutine
