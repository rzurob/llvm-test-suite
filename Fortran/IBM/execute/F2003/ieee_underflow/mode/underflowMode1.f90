! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 15 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_set_underflow_mode(gradual)
!*                             : ieee_get_underflow_mode(gradual)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*  test if above 2 intrinsics conform with Fortran 2003 standard from argument perspective, pass different valid arguments to SET and GET subroutines
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      program underflowMode1
         use,intrinsic :: ieee_arithmetic
         implicit none

         logical :: underflowmode
         logical,pointer :: p_underflowmode=>null()
         logical,allocatable ::a_underflowmode
         real    :: r
         double precision :: d
         integer,parameter :: k = kind(underflowmode)
         logical :: mode1,mode2,mode3,mode4
         logical,parameter :: mode5=.true.,mode6=.false.
         logical,pointer   :: lp1=>null(),lp2=>null()
         logical,allocatable  :: la1,la2

         data mode1,mode2,mode3,mode4 /.true.,.false.,T,F/
         allocate(a_underflowmode)
         allocate(lp1,lp2)
         allocate(la1,la2)
         lp1=.true.
         lp2=.false.
         la1=.true.
         la2=.false.

!        pass a default logical argument to SET subroutine
!        pass a default logical argument to GET subroutine
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(mode1)
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 101_4
            call ieee_set_underflow_mode(mode2)
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 102_4
            error stop 103_4
         endif

!        pass a default logical argument(abbreviated form) to SET subroutine
!        pass a pointer of default logical argument to GET subroutine
         if(ieee_support_underflow_control()) then
            call ieee_set_underflow_mode(mode3)
            call ieee_get_underflow_mode(p_underflowmode)
            if(underflowmode .neqv. .true.) error stop 104_4
            call ieee_set_underflow_mode(mode4)
            call ieee_get_underflow_mode(p_underflowmode)
            if(underflowmode .neqv. .false.) error stop 105_4
            error stop 106_4
         endif

!        pass a default logical named constant argument to SET subroutine
!        pass an allocatable of default logical argument to GET subroutine
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(mode5)
            call ieee_get_underflow_mode(a_underflowmode)
            if(underflowmode .neqv. .true.) error stop 107_4
            call ieee_set_underflow_mode(mode6)
            call ieee_get_underflow_mode(a_underflowmode)
            if(underflowmode .neqv. .false.) error stop 108_4
            error stop 109_4
         endif

!        pass a default logical literal constant argument to SET subroutine
!        pass a default logical argument to GET subroutine
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(.true.)
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 110_4
            call ieee_set_underflow_mode(.false.)
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 111_4
            error stop 112_4
         endif

!        pass a logical literal constant argument to SET subroutine
!        pass a pointer of default logical argument to GET subroutine
         if(ieee_support_underflow_control(d)) then
            call ieee_set_underflow_mode(.true._k)
            call ieee_get_underflow_mode(p_underflowmode)
            if(underflowmode .neqv. .true.) error stop 113_4
            call ieee_set_underflow_mode(.false._k)
            call ieee_get_underflow_mode(p_underflowmode)
            if(underflowmode .neqv. .false.) error stop 114_4
            error stop 115_4
         endif

!        pass a pointer of default logical argument to SET subroutine
!        pass an allocatable of default logical argument to GET subroutine
         if(ieee_support_underflow_control(2.5)) then
            call ieee_set_underflow_mode(lp1)
            call ieee_get_underflow_mode(a_underflowmode)
            if(underflowmode .neqv. .true.) error stop 116_4
            call ieee_set_underflow_mode(lp2)
            call ieee_get_underflow_mode(a_underflowmode)
            if(underflowmode .neqv. .false.) error stop 117_4
            error stop 118_4
         endif

!        pass an allocatable of default logical argument to SET subroutine
!        pass a default logical argument to GET subroutine
         if(ieee_support_underflow_control()) then
            call ieee_set_underflow_mode(la1)
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 119_4
            call ieee_set_underflow_mode(la2)
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 120_4
            error stop 121_4
         endif

         deallocate(lp1,lp2)
         deallocate(la1,la2)
         nullify(lp1,lp2)
       end program
