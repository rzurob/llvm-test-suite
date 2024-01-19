!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on IMPLICIT array data types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,implicit
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: ASSOCIATE with expression and with
!*                                     single dimention array
!*                                     with using implicit type with
!*                                     integer*(1,2,4), logical*(1,2,4)
!*                                     real*(4,8,16), double presion
!*                                     byte and character data types.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

@PROCESS CTYPLSS

      program fxass105

      implicit integer*1        ( b )
      implicit integer*2        ( c )
      implicit integer*4        ( d )

      implicit logical          ( e )
      implicit logical*1        ( f )
      implicit logical*2        ( g )
      implicit logical*4        ( h )

      implicit real             ( i )
      implicit real*4           ( j )
      implicit real*8           ( k )
      implicit real*16          ( l )
      implicit double precision ( m )

      implicit complex          ( n )
      implicit complex*8        ( o )
      implicit complex*16       ( p )
      implicit complex*32       ( q )
      implicit double complex   ( r )

      implicit byte             ( s )

      implicit character*5      ( t )

      dimension aval( 3 )
      dimension bval( 3 )
      dimension cval( 3 )
      dimension dval( 3 )
      dimension eval( 3 )
      dimension fval( 3 )
      dimension gval( 3 )
      dimension hval( 3 )
      dimension ival( 3 )
      dimension jval( 3 )
      dimension kval( 3 )
      dimension lval( 3 )
      dimension mval( 3 )
      dimension nval( 3 )
      dimension oval( 3 )
      dimension pval( 3 )
      dimension qval( 3 )
      dimension rval( 3 )
      dimension sval( 3 )
      dimension tval( 3 )

      logical :: precision_r4, precision_r8, precision_r6
      logical :: precision_x3, precision_x6, precision_x8

      aval( 1 ) = 1
      bval( 1 ) = 2
      cval( 1 ) = 3
      dval( 1 ) = 4
      eval( 1 ) = .true.
      fval( 1 ) = .false.
      gval( 1 ) = .true.
      hval( 1 ) = .true.
      ival( 1 ) = 9.0e0
      jval( 1 ) = 10.0e0
      kval( 1 ) = 11.0d0
      lval( 1 ) = 12.0q0
      mval( 1 ) = 13.0d0
      nval( 1 ) = (14.0e0, 14.0e0)
      oval( 1 ) = (15.0e0, 15.0e0)
      pval( 1 ) = (16.0d0, 16.0d0)
      qval( 1 ) = (17.0q0, 17.0q0)
      rval( 1 ) = (18.0d0, 18.0d0)
      sval( 1 ) = 19
      tval( 1 ) = 'abcde'

      aval( 2 ) = 1
      bval( 2 ) = 2
      cval( 2 ) = 3
      dval( 2 ) = 4
      eval( 2 ) = .true.
      fval( 2 ) = .true.
      gval( 2 ) = .true.
      hval( 2 ) = .true.
      ival( 2 ) = 9.0e0
      jval( 2 ) = 10.0e0
      kval( 2 ) = 11.0d0
      lval( 2 ) = 12.0q0
      mval( 2 ) = 13.0d0
      nval( 2 ) = (14.0e0, 14.0e0)
      oval( 2 ) = (15.0e0, 15.0e0)
      pval( 2 ) = (16.0d0, 16.0d0)
      qval( 2 ) = (17.0q0, 17.0q0)
      rval( 2 ) = (18.0d0, 18.0d0)
      sval( 2 ) = 19
      tval( 2 ) = '12345'

      aval( 3 ) = 1
      bval( 3 ) = 2
      cval( 3 ) = 3
      dval( 3 ) = 4
      eval( 3 ) = .true.
      fval( 3 ) = .true.
      gval( 3 ) = .true.
      hval( 3 ) = .true.
      ival( 3 ) = 9.0e0
      jval( 3 ) = 10.0e0
      kval( 3 ) = 11.0d0
      lval( 3 ) = 12.0q0
      mval( 3 ) = 13.0d0
      nval( 3 ) = (14.0e0, 14.0e0)
      oval( 3 ) = (15.0e0, 15.0e0)
      pval( 3 ) = (16.0d0, 16.0d0)
      pval( 3 ) = (16.0d0, 16.0d0)
      qval( 3 ) = (17.0q0, 17.0q0)
      rval( 3 ) = (18.0d0, 18.0d0)
      sval( 3 ) = 19
      tval( 3 ) = 'ABCDE'

      d = dval( 1 ) + dval( 2 ) - dval( 3 )
      associate ( value1 => dval( 1 ) + dval( 2 ) - dval( 3 ) )
           if(value1 .ne. d)then
           error stop 1
           endif
      end associate

      associate ( value2 => (hval( 1 ) .eqv. hval( 2 ) .eqv. hval( 3 )) )
           if(value2 .neqv. .true.)then
           error stop 2
           endif
      end associate

      m =  mval( 1 ) + mval( 2 ) + mval( 3 )
      associate ( value3 => mval( 1 ) + mval( 2 ) + mval( 3 ) )
           if(.not. precision_r8(value3,m)) then
           error stop 3
           endif
      end associate

      r = rval( 1 ) + rval( 2 ) - rval( 3 )
      associate ( value4 => rval( 1 ) + rval( 2 ) - rval( 3 ) )
           if (.not.precision_x6(value4,r)) then
           error stop 4
           endif
      end associate

      s = sval( 1 ) + sval( 2 ) - sval( 3 )
      associate ( value5 => sval( 1 ) + sval( 2 ) - sval( 3 ) )
           if(value5 .ne. s)then
           error stop 5
           endif
      end associate

      associate ( value6 => tval( 1 ) // tval( 2 ) // tval( 3 ) )
           if(value6 .ne. 'abcde12345ABCDE')then
           error stop 6
           endif
      end associate

      end
