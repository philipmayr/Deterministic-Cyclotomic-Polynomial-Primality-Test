' Deterministic Cyclotomic Polynomial Primality Test

Imports System

Module DeterministicCyclotomicPolynomialPrimalityTest

    Function TestExponentiality(CandidatePower As Integer) As Boolean
        If CandidatePower < 2 Then Return False

        Dim GreatestExponent As Integer = CInt(Math.Floor(Math.Log(CandidatePower) / Math.Log(2)))

        For Exponent As Integer = 2 To GreatestExponent
            Dim CandidateBase As Integer = CInt(Math.Round(Math.Pow(CandidatePower, 1.0 / Exponent)))

            If Math.Pow(CandidateBase, Exponent) = CandidatePower Then
                Return True
            End If
        Next

        Return False
    End Function

    Function FindGreatestCommonDivisor(FirstNumber As Integer, SecondNumber As Integer) As Integer
        If SecondNumber > 0 Then
            Return FindGreatestCommonDivisor(SecondNumber, FirstNumber Mod SecondNumber)
        Else
            Return FirstNumber
        End If
    End Function

    Function FindLeastOrderThresholdModulus(Number As Integer) As Integer
        Dim Threshold As Double = Math.Pow(Math.Log(Number, 2), 2)

        For LeastOrderThresholdModulusCandidate As Integer = 2 To Number
            If FindGreatestCommonDivisor(Number, LeastOrderThresholdModulusCandidate) = 1 Then
                Dim MultiplicativeOrder As Integer = FindMultiplicativeOrder(Number, LeastOrderThresholdModulusCandidate)

                If MultiplicativeOrder > Threshold Then
                    Return LeastOrderThresholdModulusCandidate
                End If
            End If
        Next

        Return -1
    End Function

    Function FindMultiplicativeOrder(BaseNumber As Integer, Modulus As Integer) As Integer
        If FindGreatestCommonDivisor(BaseNumber, Modulus) <> 1 Then Return -1

        Dim Index As Integer = 1
        Dim Residue As Integer = BaseNumber Mod Modulus

        While Residue <> 1
            Residue = (Residue * BaseNumber) Mod Modulus
            Index += 1

            If Index > Modulus Then Return -1
        End While

        Return Index
    End Function

    Function TestPrimality(PrimeCandidate As Integer) As Boolean
        If TestExponentiality(PrimeCandidate) Then
            Return False
        End If

        Dim LeastOrderThresholdModulus As Integer = FindLeastOrderThresholdModulus(PrimeCandidate)

        If PrimeCandidate <= LeastOrderThresholdModulus Then
            Return True
        End If

        For Number As Integer = 2 To LeastOrderThresholdModulus
            If FindGreatestCommonDivisor(Number, PrimeCandidate) > 1 Then
                Return False
            End If
        Next

        Dim PhiOfLeastOrderThresholdModulus As Integer = FindTotient(LeastOrderThresholdModulus)

        Dim BasePolynomial(LeastOrderThresholdModulus - 1) As Long

        Dim UpperBound = CInt(Math.Floor(Math.Sqrt(PhiOfLeastOrderThresholdModulus) * Math.Log(PrimeCandidate, 2)))

        ' Run Polynomial Congruence Test

        For ConstantTerm As Integer = 1 To UpperBound
            Array.Clear(BasePolynomial, 0, LeastOrderThresholdModulus)
            BasePolynomial(0) = ConstantTerm
            BasePolynomial(1) = 1

            Dim LeftHandSide = ExponentiatePolynomialModularly(BasePolynomial, PrimeCandidate, PrimeCandidate, LeastOrderThresholdModulus)

            Dim RightHandSide(LeastOrderThresholdModulus - 1) As Long
            Dim ReducedExponent = CInt(PrimeCandidate Mod LeastOrderThresholdModulus)

            RightHandSide(ReducedExponent) = 1
            RightHandSide(0) = ConstantTerm

            For i As Integer = 0 To LeastOrderThresholdModulus - 1
                If LeftHandSide(i) <> RightHandSide(i) Then
                    Return False
                End If
            Next
        Next

        Return True
    End Function

    Function FindTotient(Number As Integer) As Integer
        Dim Count As Integer = 0

        For CoprimeCandidate As Integer = 1 To Number
            If FindGreatestCommonDivisor(CoprimeCandidate, Number) = 1 Then Count += 1
        Next

        Return Count
    End Function

    Function ExponentiatePolynomialModularly(Polynomial As Long(), Exponent As Long, IntegerModulus As Long, DegreeBound As Integer) As Long()
        Dim PolynomialResidue(DegreeBound - 1) As Long
        PolynomialResidue(0) = 1

        Dim BasePolynomial = CType(Polynomial.Clone(), Long())

        While Exponent > 0
            If (Exponent And 1) = 1 Then
                PolynomialResidue = MultiplyPolynomials(PolynomialResidue, BasePolynomial, IntegerModulus, DegreeBound)
            End If

            BasePolynomial = MultiplyPolynomials(BasePolynomial, BasePolynomial, IntegerModulus, DegreeBound)
            Exponent = Exponent >> 1
        End While

        Return PolynomialResidue
    End Function

    Function MultiplyPolynomials(MultiplierPolynomialCoefficients As Long(), MultiplicandPolynomialCoefficients As Long(), IntegerModulus As Long, DegreeBound As Integer) As Long()
        Dim ProductPolynomial(DegreeBound - 1) As Long

        For MultiplierPolynomialTerm As Integer = 0 To DegreeBound - 1
            If MultiplierPolynomialCoefficients(MultiplierPolynomialTerm) = 0 Then Continue For

            For MultiplicandPolynomialTerm As Integer = 0 To DegreeBound - 1
                If MultiplicandPolynomialCoefficients(MultiplicandPolynomialTerm) = 0 Then Continue For

                Dim ProductPolynomialTerm = (MultiplierPolynomialTerm + MultiplicandPolynomialTerm) Mod DegreeBound

                ProductPolynomial(ProductPolynomialTerm) = (ProductPolynomial(ProductPolynomialTerm) + MultiplierPolynomialCoefficients(MultiplierPolynomialTerm) * MultiplicandPolynomialCoefficients(MultiplicandPolynomialTerm)) Mod IntegerModulus
            Next
        Next

        Return ProductPolynomial
    End Function

    Sub Main()
        ' Dim ListOfPerfectPowers() As Integer = {1, 4, 8, 9, 10, 27, 32, 36, 64, 100, 125, 144, 169, 200}

        ' For Each Number As Integer In ListOfPerfectPowers
        '     Console.WriteLine(Number.ToString() & " is " & If(TestExponentiality(Number), "a perfect power.", "not a perfect power."))
        ' Next

        Console.Write("Enter an integer to test for primality: ")
        Dim InputString As String = Console.ReadLine()
    
        Dim PrimeCandidate As Long
        If Long.TryParse(InputString, PrimeCandidate) Then
            Dim Primality As Boolean = TestPrimality(PrimeCandidate)
    
            Console.WriteLine(PrimeCandidate.ToString() & " is " & If(Primality, "a prime number.", "not a prime number."))
        Else
            Console.WriteLine("Invalid input. Please enter a valid integer.")
        End If
    End Sub

End Module
