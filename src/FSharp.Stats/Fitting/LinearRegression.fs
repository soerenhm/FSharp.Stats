namespace FSharp.Stats.Fitting


(*
Linear regression estimates the relationship of one variable (A) to another (B) by expressing B in terms of a linear function of A.
*)

module LinearRegression =    

    open FSharp.Stats

    module OrdinaryLeastSquares = 
          
        /// Simple linear regression y : x -> a + bx
        module Linear = 
        
            /// Regression through the origin (y : x -> bx)
            module RTO =
            
                /// Calculates the slope for simple linear regression through the origin.
                let fit (x: Vector<float>) (y: Vector<float>) =
                    if x.Length <> y.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let numerator   = Seq.zip x y |> Seq.sumBy (fun (x,y) -> x * y)
                    let denominator = x |> Seq.sumBy (fun x -> x * x)
                    numerator / denominator

                /// Calculates the function value with the given coefficients and x value.
                let predict (coef: float) (x:float) =            
                    coef * x

                //todo 
                //Multivariable

            module Univariable =    
                /// Calculates the coefficients for linear regression
                /// in the form of [|intercept; slope;|]
                let fit (xData: Vector<float>) (yData: Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    Algebra.LinearAlgebra.LeastSquares X yData

                /// Calculates the coefficients for linear regression through a specified point (xC,yC) 
                /// in the form of [|intercept; slope;|]
                let fitConstrained (xData: Vector<float>) (yData: Vector<float>) ((xC,yC): float*float) =
                    let xTransformed = xData |> Vector.map (fun x -> x - xC)
                    let yTransformed = yData |> Vector.map (fun y -> y - yC)
                    let slope = RTO.coefficientOfVector xTransformed yTransformed
                    [|- xC * slope - yC;slope|]

                /// Calculates the function value with the given coefficients and x value.
                let predict (coef : Vector<float>) (x:float) =
                    if coef.Length <> 2 then
                        raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                    coef.[0] + coef.[1] * x
        
                /// Fits a model (y(x) = b + m * x) to the data and returns the cooks distance for every data pair present in the
                /// input collections as an estimator for the influence of each data point in coefficient estimation.  
                let cooksDistance (xData: Vector<float>) (yData: Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    let coeffs = Algebra.LinearAlgebra.LeastSquares X yData
                    let leverages = Algebra.LinearAlgebra.leverage X
                    let yPred = Vector.map (fit coeffs) xData
                    let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                    let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                    // compute cooksDistance for every Point in the dataSet
                    squaredDeviations 
                    |> FSharp.Stats.Vector.mapi (fun i squaredDev -> 
                        let fstFactor = squaredDev / (MSE * float coeffs.Length)
                        let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                        fstFactor * sndFactor
                        )

            module Multivariable =           
                /// Calculates the coefficients for linear regression
                /// in the form of [|intercept; slope;|]
                let fit (xData: Matrix<float>) (yData: Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = xData.NumRows
                    let n = xData.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
                    Algebra.LinearAlgebra.LeastSquares X yData
                    
                /// Calculates the function value with the given coefficients and x value.
                let predict (coef: Vector<float>) (x: Vector<float>) =
                    let tmp: Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef 
            
            module RidgeRegression =           

                let fit lambda (xData: Matrix<float>) (yData: Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = xData.NumRows
                    let n = xData.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
                    
                    let lambdaIdentity = lambda * Matrix.identity n
                    let sumDot = X.Transpose * X + lambdaIdentity
                    let theInverse = Algebra.LinearAlgebra.Inverse sumDot
                    let inverseXt = theInverse * X.Transpose
                    let w = inverseXt * yData
 
                    w

                /// Calculates the function value with the given coefficients and x value.
                let predict (coef: Vector<float>) (x: Vector<float>) =
                    let tmp: Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef 


        /// Simple polynomial regression
        module Polynomial =
            
            let private vandermondeRow order (x: float) = 
                Vector.init (order+1) (fun i -> pown x i)        

            let private vandermondeMatrix order (vec: Vector<float>) =        
                Matrix.init vec.Length (order+1) (fun m order -> pown vec.[m] order) 
            
            /// Calculates the coefficients for polynomial regression (ax*bx^2*cx^3...)
            /// in the form of [|a; b; c; ...|].
            let fit order (xData: Vector<float>) (yData: Vector<float>) =
                if xData.Length <> yData.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = xData.Length
                let A = vandermondeMatrix order xData
                // Least Squares of |y=A(x)*c| 
                //  tr(A)*y = tr(A)*A*c
                //  inv(tr(A)*A)*tr(A)*y = c        
                let AtA = A.Transpose * A
                let Aty = A.Transpose * yData
                Algebra.LinearAlgebra.LeastSquares AtA Aty        

            /// Calculates the coefficients for polynomial regression (ax*bx^2*cx^3...) with given weighting
            /// in the form of [|a; b; c; ...|].
            let fitWithWeighting order (weighting: Vector<float>) (xData: Vector<float>) (yData: Vector<float>) = 
                if xData.Length <> yData.Length || xData.Length <> weighting.Length then
                    raise (System.ArgumentException("vector x,y and weighting have to be the same size!"))
                let A = 
                    let includeWeighting weighting order =
                        Matrix.init (order + 1) (order + 1) 
                            (fun i j -> 
                                Vector.map2 (fun x w -> w * (pown x (i + j))) xData weighting 
                                |> Vector.sum
                            )
                    includeWeighting weighting order
                let b = 
                    Vector.init (order + 1) 
                        (fun i -> 
                            Vector.map3 (fun x y w -> w * (pown x i) * y) xData yData weighting 
                            |> Vector.sum
                        )
                Algebra.LinearAlgebra.SolveLinearSystem A b   

            /////takes vector of data with n>1 replicates and gives a vector of weightings based on the variance in measurements ( 1/var(i..j) )
            /////only apply if y > 0 !
            //let getWeightingOfVariance numberOfReplicates (yData:Vector<float>) =
            //    let var =
            //        if yData.Length % numberOfReplicates = 0 then
            //            let length = yData.Length / numberOfReplicates
            //            let variance = vector [for i = 0 to length-1 do yield yData.[i * numberOfReplicates .. (i + 1) * numberOfReplicates - 1] |> Seq.var]
            //            variance
            //        else raise (System.ArgumentException("data length no multiple of replicate number!")) 
            //    Vector.init (yData.Length / numberOfReplicates) (fun i -> 1. / var.[i])
                        
            /// Calculates the function value with the given coefficients and x value.
            let predict order (coef: Vector<float>) (x: float) =            
                Vector.dot coef (vandermondeRow order x)

            /// Calculates derivative at x with given polynomial coefficients. Level1 = fst derivative; Level2 = snd derivative ...
            let getDerivative (*(order: int)*) (coef: Vector<float>) (level: int) (x: float) =
                let order = coef.Length - 1
                Array.init (order + 1) (fun i -> 
                    let factor = 
                        List.init level (fun l -> i-l)
                        |> List.fold (fun acc c -> acc * (float c)) 1.
                    factor * coef.[i] * (pown x (i-level))
                    )
                |> Array.sum
                
            /// Fits a polynomial model of user defined order to the data and returns the cooks distance for every data pair present in the
            /// input collections as an estimator for the influence of each data point in coefficient estimation.  
            let cooksDistance order (xData: Vector<float>) (yData: Vector<float>) =
                if xData.Length <> yData.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = xData.Length
                let A = vandermondeMatrix order xData
                let coeffs = Algebra.LinearAlgebra.LeastSquares A yData
                let leverages = Algebra.LinearAlgebra.leverage A
                let yPred = Vector.map (fit order coeffs) xData
                let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                // compute cooksDistance for every Point in the dataSet
                squaredDeviations 
                |> Vector.mapi (fun i squaredDev -> 
                    let fstFactor = squaredDev / (MSE * float coeffs.Length)
                    let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                    fstFactor * sndFactor)
                                   
            // <summary>
            // Find the model parameters ? such that X*? with predictor X becomes as close to response Y as possible, with least squares residuals.
            // Uses a singular value decomposition and is therefore more numerically stable (especially if ill-conditioned) than the normal equations or QR but also slower.
            // </summary>            
    
    module RobustRegression =
        
        /// robust linear regression y : x -> a + bx
        module Linear =

            /// Calculates theil's incomplete method for simple linear regression in the form of [|intercept; slope|]
            let fitTheilEstimator (xData: Vector<float>) (yData: Vector<float>) = 
                //sort data in ascending order (xData)
                let data =
                    Array.zip (Vector.toArray xData) (Vector.toArray yData)
                    |> Array.sortBy fst
                
                //low/high group separation. If n is odd, the central data point is ignored
                let (low,high) =
                    let length = data.Length

                    if length <= 1 then 
                        raise (System.ArgumentException("input vector is too small"))

                    match length % 2 with
                    | 1 -> data.[..(length / 2 - 1)],data.[(length / 2 + 1)..]
                    | _ -> data.[..(length / 2 - 1)],data.[(length / 2)..]

                let slope =
                    low
                    |> Array.mapi (fun i (xL,yL) -> 
                        let (xH,yH) = high.[i]
                        //calculate slope
                        (yH - yL) / (xH - xL)
                        )
                    |> Array.median

                let intercept =
                    data
                    |> Array.map (fun (xV,yV) -> yV - (slope * xV))
                    |> Array.median

                vector [|intercept;slope|]


            /// Calculates the robust Theil-Sen estimator for simple linear regression in the form of [|intercept; slope;|]
            let fitTheilSenEstimator (xData: Vector<float>) (yData: Vector<float>) =
                let xLength = xData.Length

                let indicesOfUniqueOccurences =
                    let rec loop acc i =
                        if i < xLength then 
                            let tmp = xData.[i]
                            let occurences =
                                xData
                                |> Seq.filter (fun xT -> tmp = xT)
                            if Seq.length occurences > 1 
                                then loop acc (i+1)
                            else loop (i::acc) (i+1)
                        else acc
                    loop [] 0

                let isolateUnique (data: Vector<float>) =
                    indicesOfUniqueOccurences
                    |> List.map (fun i -> data.[i])
                    |> vector

                let filteredXData = isolateUnique xData
                let filteredYData = isolateUnique yData
                theilEstimator filteredXData filteredYData

            let predict = OrdinaryLeastSquares.Linear.Univariable.fit

    
    type Contraint =
        | Unconstrained
        | RegressionThroughOrigin

    type FittingMethod = 
        | SimpleLinear of Constraint
        | Polynomial of int
        | Robust

    let fit (fittingMethod: FittingMethod) =
        match fittingMethod with 
        | SimpleLinear c -> 
            match c with
            | Unconstrained -> OrdinaryLeastSquares.Linear.Univariable.coefficient 
            // whats with multivarible (refer everything to multivarible!)
            | Contraint.RegressionThroughOrigin -> OrdinaryLeastSquares.Linear.RTO.coefficient
        | Polynomial o -> OrdinaryLeastSquares.Polynomial.coefficient o
            
    let predict (fittingMethod: FittingMethod)  = 
        match fittingMethod with 
        | SimpleLinear c -> 
            match c with
            | Unconstrained -> OrdinaryLeastSquares.Linear.Univariable.fit 
            // whats with multivarible (refer everything to multivarible!)
            | Contraint.RegressionThroughOrigin -> OrdinaryLeastSquares.Linear.RTO.fit
        | Polynomial o -> OrdinaryLeastSquares.Polynomial.fit o


(*
preferred structure:
multivariable linar (polynomial) regression
    -> submodule for simple linear
    -> submodule for univariable
robust regression as extra module
*)

