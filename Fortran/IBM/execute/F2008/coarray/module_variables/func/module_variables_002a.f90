! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : September 29, 2010
!* .or.GIN                     :
!*                             : 
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test module variables for array: INTEGER, REAL,
!                                CHARACTER, COMPLEX, LOGICAL
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_002a

	use mod001f

	implicit none
    
	integer	:: array1(10)
	real    :: array2(10)
	complex :: cm
	character :: ch
	
	integer :: me,num,i, left
	
	integer*8	:: ci
	real*8		:: cr
	
	me = THIS_IMAGE()
    num = NUM_IMAGES()

    !-Test module variables for array: INTEGER/REAL--
   	array1 = [(i*me, i = 1,10)]
	array2 = [(i*me*1.0, i = 1,10)]
    ar_int1(:)[me] = array1(:)
	ar_re1(:)[me] = array1(:)
	ci = PRODUCT(ar_int1[me])
	cr = PRODUCT(ar_re1[me])
	print *, me, ci, cr
	
	!Expectedresult for 'print *, me, ci' is
	!
	!4 3805072588800
	!2 3715891200
	!10 36288000000000000
	!1 3628800
	!6 219419659468800
	!9 12652843234348800
	!3 214277011200
	!8 3896394330931200
	!7 1025046183571200
	!5 35437500000000
	!
	!Expected result for 'print *, me, cr' is
	!5 35437500000000.0000
	!10 36288000000000000.0
	!9 12652843234348800.0
	!3 214277011200.000000
	!6 219419659468800.000
	!7 1025046183571200.00
	!2 3715891200.00000000
	!8 3896394330931200.00
	!4 3805072588800.00000
	!1 3628800.00000000000
	!---------------------------------------------------
	
	!----Test module variables for array: COMPLEX------
	ar_comp1[me] = (/(1.0,1.0*me),(2.0,2.0*me),(3.0,3.0*me),(4.0,4.0*me),(5.0,5.0*me),(6.0,6.0*me),(7.0,7.0*me),(8.0,8.0*me),(9.0,9.0*me),(10.0,10.0*me)/)
	
	cm = PRODUCT(ar_comp1[me])
	
	print *, me, cm	
	!---------------------------------------------------
	
	!----Test module variables for array: LOGICAL------
	ar_log1[me] = [((mod(i,me) .ne. 0), i = 1, 10)]
		
	print *, me, ar_log1(me)[me]
	
	!Expected result for 'print *, me, caf6(me)[me]' is
	!6 F
	!10 F
	!1 F
	!3 F
	!9 F
	!2 F
	!7 F
	!5 F
	!4 F
	!8 F
	!----------------------------------------------------
	
	!----Test module variables for array: CHARACTER------
	if (me == 1) then
        left = num
    else
        left = me - 1
    end if
	
	ar_char1(1)[me] = 'a'
	ar_char2(1)[me] = 'a'
	
	ch = ar_char1(1)[me]
	ar_char1(1)[me] = ar_char2(1)[left]
	ar_char2(1)[me] = ch
	sync all
	
	print *,me, ar_char1(1)[me], ar_char2(1)[me]
	
	!Expected result for 'print *,me, ar_char1(1)[me], ar_char2(1)[me]' is
	!4 ba
	!5 ba
	!3 ba
	!2 ba
	!1 ba
	!----------------------------------------------------
	
	
	
	
end program module_variables_002a	
