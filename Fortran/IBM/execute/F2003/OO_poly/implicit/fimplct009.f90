!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct009.f
! %VERIFY:
! %STDIN:
! %STDOUT: fimplct009.out
! %EXECARGS:
! %POSTCMD: spiff -r1.e-6 fimplct009.out $TR_SRC/fimplct009.vf && rm -f fimplct009.out
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fimplct009

implicit real (j)


associate (x => j1)
    x = 1.5
    print *, x, j1
end associate

end
