!Copyright (C) 2007 Ashwith Jerome Rego
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.


!Finding Solutions of a Quadratic Equation
!Author : Louisda16th a.k.a Ashwith J. Rego
!Description:
!This program calculates the roots of a quadratic equation based on the quadratic formula:
!x= (-b +/- (b*b - 4*a*c)^(1/))/(2*a)

PROGRAM quadratic
!Declarations
        IMPLICIT NONE
        REAL, PARAMETER :: zero_limit=0.5e-7 !Number below this are assumed as zero due to rounding errors
        REAL :: a,b,c,discr,x, x1, x2, sqrt_discr !Variables to store coefficients and roots
        
        PRINT*,"Type values of a, b, c"
        READ*, a,b,c
        
        !Check if equation is linear
        IF (a == 0) THEN
          PRINT*,"Equation is linear. Enter a quadratic equation (a should not be 0)"
          RETURN
        ENDIF
        
        !Calculate Discriminant
        discr=b*b-4*a*c
           
        IF(discr<0) THEN         !Calculte imaginary roots as discriminant is 0
                discr = -discr
                x = 0.5*SQRT(discr)/a
                x1 = -b*.5/a
                PRINT*,"x1 = ",x1," + ",x,"i"
                PRINT*,"x2 = ",x1," - ",x,"i"
        ELSE IF(ABS(discr)<zero_limit) THEN  !Roots are real and equal as discriminant is 0
                x = -b*0.5/a
                PRINT*,"x1 = x2 =",x
        ELSE                                !Roots are real and distinct
                sqrt_discr = SQRT(discr)
                x1 = (-b + sqrt_discr)*0.5/a
                x2 = (-b - sqrt_discr)*0.5/a
                PRINT*,"x1 = ",x1
                PRINT*,"x2 = ",x2
        ENDIF
END PROGRAM quadratic
