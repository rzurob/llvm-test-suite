module mod001f

	implicit none

!************************************
!	Integer			-				!
!	Real			-				!
!	Complex			-				!
!	Character		-				!
!	Logical			-				!
!	Arrays			-				!
!	Derived Types	-				!
!************************************

!************************************
! BEGIN:	Integer Variables		!
!************************************

	integer(2), save	:: int02[*]
	integer(4), save	:: int04[*]
	integer(8), save	:: int08[*]

!************************************
! END:		Integer Variables		!
!************************************

!************************************
! BEGIN:	Real Variables			!
!************************************

	real, 	 save	:: real02[*]
	real(4), save	:: real04[*]
	real(8), save	:: real08[*]

!************************************
! END:		Real Variables			!
!************************************

!************************************
! BEGIN:	Complex					!
!************************************

	complex, save		:: comp1[*]
	complex, save		:: comp2[*]

!************************************
! END:		Complex					!
!************************************

!************************************
! BEGIN:	Character				!
!************************************

	!character,	save	:: char02[*]
	character,	ALLOCATABLE	:: char02[*]

!************************************
! END:		Character				!
!************************************

!************************************
! BEGIN:	Logical  				!
!************************************

	logical(2), save	:: log02[*]
	logical(4), save	:: log04[*]
	logical(8), save	:: log08[*]

!************************************
! END:		Logical  				!
!************************************

!************************************
! BEGIN:	Arrays  				!
!************************************
	integer*8,	save	:: ar_int1(10)[*]
	real*8,		save 	:: ar_re1(10)[*]
	complex,	save	:: ar_comp1(10)[*]
	character,	save	:: ar_char1(10)[*], ar_char2(10)[*]
	logical,	save	:: ar_log1(10)[*]

!************************************
! END:		Arrays  				!
!************************************

!************************************
! BEGIN:	Variables to test:		!
!			USE  module-name, ONLY: !
!************************************
	integer*8	:: Ai,Bi,Ci,Di
	real*8	 	:: Ar,Br,Cr,Dr
	character,	save	:: Ach,Bch,Cch,Dch
	logical*2 ::	Al,Bl,Cl,Dl

!************************************
! END:								!
!************************************

end module mod001f
