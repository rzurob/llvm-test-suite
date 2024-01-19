! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 15 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control(X)
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*  DESCRIPTION                :
!*  test if ieee_support_underflow_control(X) returns false when passing
!*  an intrinsic function as an argument
!*
!23456789012345678901234567890123456789012345678901234567890123456789012
      program underflowCtrl5
         use,intrinsic :: ieee_arithmetic

!        pass REAL(A,KIND) as an argument
         if ( ieee_support_underflow_control( &
              real( (3.2,2.1),kind(3.0) )   ) )             error stop 101_4

!        pass RAND() as an argument
         if ( ieee_support_underflow_control(5.3*rand()) )  error stop 102_4

!        pass RESHAPE(SOURCE,SHAPE) as an argument
         if ( ieee_support_underflow_control( &
           reshape((/1.0,2.1,3.4,5.2/),(/2,2/))) )          error stop 103_4

!        pass MIN(A1,A2,A3,...) as an argument
         if ( ieee_support_underflow_control( &
              min(2.78,3.1,-2.9,0.6) ) )                    error stop 104_4

       end program
