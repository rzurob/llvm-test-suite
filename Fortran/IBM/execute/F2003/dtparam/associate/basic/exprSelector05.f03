!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July 14, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an Expression
!*  SECONDARY FUNCTIONS TESTED : The Expression is a Level-2 Expression
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  An expr that contains a:
!*     -  Level-1 to Level-5 Expression
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod

    TYPE str(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        CHARACTER(l) :: str
        INTEGER(k) :: pos = 1

        CONTAINS

            PROCEDURE :: Parse
            GENERIC :: OPERATOR(/) => Parse

    END TYPE str

    CONTAINS

        FUNCTION Parse(this, delimiter)
            CLASS(str(4,*)), INTENT(in) :: this
            CLASS(str(4,*)), INTENT(in) :: delimiter
            TYPE(str(4,:)), POINTER :: Parse

            INTEGER :: endToken

            endToken = index(this%str( this%pos: ), delimiter%str( 1:1 ))
            IF (endToken == 0) THEN
                endToken = LEN( this%str )
            ELSE
                endToken = this%pos + endToken - 2
            END IF

            ALLOCATE(str(4,endToken-this%pos+1) :: Parse)
            Parse = str(4,endToken-this%pos+1)(this%str( this%pos:endToken ))

        END FUNCTION Parse

END MODULE mod


PROGRAM exprSelector05
    USE mod

    IMPLICIT NONE


    INTEGER :: i = 0
    INTEGER :: tokenKinds( 3 )

    CHARACTER(5), PARAMETER :: hello = 'Hello'
    CHARACTER(5), PARAMETER :: world = 'World'
    CHARACTER(1), PARAMETER :: bang = '!'

    CHARACTER(13), PARAMETER :: helloWorld =&
                                    hello // '-' // world // '-' // bang

    CHARACTER(5) :: found( 3 )
    CHARACTER(5) :: check( 3 ) = [ CHARACTER(5) :: hello, world, bang ]

    TYPE(str(4,1)) :: spaceStr = str(4,1)('-')
    TYPE(str(4,13)) :: helloWorldStr = str(4,13)(helloWorld)


    DO WHILE (helloWorldStr%pos <= LEN( helloWorldStr%str ))
        ASSOCIATE(token => (helloWorldStr / spaceStr))
            i = i + 1

            found( i ) = token%str
            tokenKinds( i ) = token%k

            helloWorldStr%pos = helloWorldStr%pos + LEN( token%str ) + 1

            PRINT *, 'Token:', token%pos, '(', token%str, '), ',&
                     'String:', helloWorldStr%pos, '(', helloWorldStr%str, ')'
        END ASSOCIATE

        IF (i > 3) THEN
            STOP 10
        END IF
    END DO

    IF ( ANY(found /= check) ) THEN
        STOP 20

    ELSE IF ( ANY(tokenKinds /= 4) ) then
        STOP 30
    END IF

END PROGRAM exprSelector05
