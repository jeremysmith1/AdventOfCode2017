module OneInverseCaptcha

open NUnit.Framework

let InverseCaptcha input= 1

[<TestFixture>]
type TestClass() =


    [<Test>]
    member this.``Assert that InverseCaptcha 1122 is 3`` ()=
        Assert.AreEqual(3, InverseCaptcha 1122)

    [<Test>]
    member this.``Assert that InverseCaptcha 1111 is 4`` ()=
        Assert.AreEqual(4, InverseCaptcha 1111)

    [<Test>]
    member this.``Assert that InverseCaptcha 1234 is 0`` ()=
        Assert.AreEqual(0, InverseCaptcha 1234)

    [<Test>]
    member this.``Assert that InverseCaptcha 91212129 is 9`` ()=
        Assert.AreEqual(9, InverseCaptcha 91212129)

    