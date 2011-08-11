package main

import (
	"fmt"
	"flag"
	"rand"
	"time"
	"big"
	"bytes"
	"strings"
	"strconv"
	"io/ioutil"
)

// parametry prikazove radky - ve tvaru (prepinac, defaultni_hodnota, popis)
var keyLength = flag.Int("b", 30, "Key length in bites")
var  publicKeyFile = flag.String("u", "pu.key", "File to store public key")
var privateKeyFile = flag.String("r", "pr.key", "File to store private key")
var generateKeys = flag.Bool("g", false, "Generate keys")
var encrypFiles = flag.Bool("e", false, "Encrypt/decript with private key")
var decrypFiles = flag.Bool("d", false, "Decrypt/encrypt with public key")
var splitFile = flag.String("s", "", "File to be split and can be encrypted/decrypted")
var joinFile  = flag.String("j", "", "Joins parts to this file")
var prefix    = flag.String("p", "msg", "Parts prefix")

// pomocna cisla
var big0 *big.Int = big.NewInt(0)
var big1 *big.Int = big.NewInt(1)

// Vrati (a - b) % n
func _Euklid_sub(a, b, n *big.Int) (z *big.Int) {
	z = new(big.Int)
	z.Sub(n, z.Mod(b,n)) // b = n - (b % n) [inverze ke scitani]
	z.Mod(z.Add(a,z), n) // z = (a + b) % n
	return
}

// Zjisti a0, b0, z tak aby: a0*x + b0*y = z (mod n)
func Euklid(x, y, n *big.Int) (a0,b0,z *big.Int) {
	a0 = big1
	a := big0
	b0 = big0
	b := big1
	g := new(big.Int)
	t := new(big.Int) // tmp

	if x.Cmp(y) < 0 {
		x, y = y, x
		a0, a, b0, b = b0, b, a0, a
	}

	for {
		z = y
		_, y = g.Div(x, y)
		x = z

		a0, a = a, _Euklid_sub(a0,t.Mul(a,g), n)
		b0, b = b, _Euklid_sub(b0,t.Mul(b,g), n)

		if y.Cmp(big0) == 0 { break }
	}
	return
}

func randBytes(len int) []byte {
	bytes := make([]byte, len)
	for i:=len-1; i>=0; i-- {
		bytes[i] = byte(rand.Uint32() % 256)
	}
	return bytes
}

// Vygeneruje nahodne cislo v intervalu [2^len, 2^len+1)
func RandNum(len int) *big.Int {
	bytes := randBytes((len+7)/8)
	s := byte(len % 8)
	bytes[0] %= 2<<s
	for bytes[0] == 0 { bytes[0] = byte(rand.Uint32() % (2<<s)) }

	return new(big.Int).SetBytes(bytes)
}

// Vygeneruje "nahodne" nenulove cislo mensi nez n
func RandNumSmaller(n *big.Int) (r *big.Int) {
	r = big.NewInt(0)
	for r.Cmp(big0) == 0 {
		bytes := randBytes(len(n.Bytes())*8)
		r.SetBytes(bytes)
		r.Mod(r,n)
	}
	return
}

func RabinMiller(p *big.Int) bool {
	pdec := new(big.Int).Sub(p, big.NewInt(1)) // =  p - 1
	big2 := big.NewInt(2)

	for i:=0; i<20; i++ {
		x := RandNumSmaller(p)
		stg := new(big.Int).Exp(x, pdec, p) // = x^(p-1) mod p
		if stg.Cmp(big1) != 0 { return false }

		// test na Carmichaelova cisla (kontrola zda x^[(p-1)/2] je +1 nebo -1)
		p2 := new(big.Int).Rsh(p, 1)        // = (p - 1)/2
		for {
			stg.Exp(x, p2, p)
			if stg.Cmp(pdec) == 0 { break }
			if stg.Cmp(big1) != 0 { return false }
			_,res := p2.Div(p2, big2)
			if res.Cmp(big1) == 0 { break }
		}
	}
	return true
}

func NewPrime(ln int) (r *big.Int) {
	var bytes []byte = nil
	r = new(big.Int)
	for bytes == nil || !RabinMiller(r) {
		bytes = randBytes((ln+7)/8)
		bytes[len(bytes)-1] |= 1 // jen licha cisla
		r.SetBytes(bytes)
	}
	return
}

// Vrati cisla n, e, d, p, q takova ze, pro nahodne vygenerovana ln-bitova
// prvocisla p, q plati, ze e*d + (p-1)(q-1)*X = 1 (mod (p-1)(q-1)) a n = p*q.
func GenerateKeys(ln int) (n,e,d,p,q *big.Int) {
	e  = big.NewInt(17) // mělo by stačit
	n  = new(big.Int)
	m := new(big.Int)
	t := new(big.Int)  // tmp
	var r *big.Int

	for {
		p = NewPrime(ln)
		q = NewPrime(ln)
		if p.Cmp(q) == 0 { continue }

		n.Mul(p,q)                            // n = p*q
		m.Mul(t.Sub(p, big1), m.Sub(q, big1)) // m = (p-1)*(q-1)
		_, d, r = Euklid(m,e,m)

		if r.Cmp(big1) != 0 { continue } // je GCD(e,m) = 1 ?
		break
	}
	return
}

func SaveKeys(n, e, d, p, q *big.Int) {
	pu := strings.Bytes(n.String() +"\n"+ e.String() +"\n")
	pr := strings.Bytes(p.String() +"\n"+ q.String() +"\n"+ d.String() +"\n")

	if ioutil.WriteFile(*publicKeyFile,  pu, 0600) != nil ||
	   ioutil.WriteFile(*privateKeyFile, pr, 0600) != nil {
		panic("Writing problems")
	}
}

func readFile(name string) []byte {
	bytes, err := ioutil.ReadFile(name)
	if err != nil { panic("Reading error") }
	return bytes
}

func writeToFile(filename string, text []byte, rights int) {
	if ioutil.WriteFile(filename, text, rights) != nil {
		panic("Writing problems")
	}
}

// Nacte public key ze souboru
func ReadPublicKey() (n, e *big.Int) {
	bytes := readFile(*publicKeyFile)
	ss := strings.Split(string(bytes), "\n", 3)
	if len(ss) < 2 { panic("Wrong Format") }

	n, b1 := new(big.Int).SetString(ss[0], 10)
	e, b2 := new(big.Int).SetString(ss[1], 10)

	if !b1 || !b2 { panic("Wrong Format") }
	return
}

// Nacte private key ze souboru
func ReadPrivateKey() (p, q, d *big.Int) {
	bytes := readFile(*privateKeyFile)
	ss := strings.Split(string(bytes), "\n", 4)
	if len(ss) < 3 { panic("Wrong Format") }

	p, b1 := new(big.Int).SetString(ss[0], 10)
	q, b2 := new(big.Int).SetString(ss[1], 10)
	d, b3 := new(big.Int).SetString(ss[2], 10)

	if !b1 || !b2 || !b3 { panic("Wrong Format") }
	return
}

// Vrati n^-1 mod p
func invMod(n, p *big.Int) *big.Int {
	return new(big.Int).Exp(n, new(big.Int).Sub(p, big.NewInt(2)), p)
}

// Spocita msg^key % mod
func crypt(key, mod *big.Int, msg []byte) []byte {
	a := new(big.Int).SetBytes(msg)
	a.Exp(a, key, mod)
	return a.Bytes()
}

// Jmeno i-te casti zpravy
func partfile(i int) string {
	return *prefix+"."+fmt.Sprintf("%d", i) // = prefix.i
}

// zjisteni poctu zasifrovanych souboru podle udaje ulozeneho v souboru prefix.info
func msgscount() int {
	max,err := strconv.Atoi(string(readFile(*prefix+".info")))
	if err != nil { panic("Wrong format") }
	return max
}

func main() {
	flag.Parse()                  // parsovani parametru prikazove radky
	rand.Seed(time.Nanoseconds()) // inicializace generatoru nahodnych cisel

	if *generateKeys {
		SaveKeys(GenerateKeys(*keyLength))
	}

	// rozdeleni souboru na vice mensich podle delky klice
	if *splitFile != "" {
		rights:= 0600
		n, _  := ReadPublicKey()
		bytes := readFile(*splitFile)
		nl := len(n.Bytes()) - 1

		i:=0
		for ; i<len(bytes)/nl; i++ {
			writeToFile(partfile(i), bytes[i*nl:(i+1)*nl], rights)
		}
		if len(bytes[i*nl:]) > 0 { // pokud deleni nevychazi presne
			writeToFile(partfile(i), bytes[i*nl:], rights)
			i++
		}

		// informacni soubor
		writeToFile(*prefix+".info", strings.Bytes(fmt.Sprintf("%d", i)), rights)
	}

	// zasifruje/rozsifruje vsechny soubory verejnym klicem
	if *encrypFiles {
		n, e := ReadPublicKey()
		max := msgscount()

		for i := 0; i<max; i++ {
			file := readFile(partfile(i))
			bs := crypt(e, n, file)
			writeToFile(partfile(i), bs, 0600)
		}
	}

	// rozsifruje/zasifruje vsechny soubory soukromym klicem
	if *decrypFiles {
		p,q,d := ReadPrivateKey()
		pinv := new(big.Int).Mul(p, invMod(p, q)) // = p*(p^-1 mod q)
		qinv := new(big.Int).Mul(q, invMod(q, p)) // = q*(q^-1 mod p)

		n := new(big.Int).Mul(p,q)
		max := msgscount()

		for i := 0; i<max; i++ {
			file := readFile(partfile(i))

			// CINSKA VETA O ZBYTCICH
			bp := new(big.Int).SetBytes(crypt(d, p, file)) // decrypt mod p
			bq := new(big.Int).SetBytes(crypt(d, q, file)) // decrypt mod q

			// bs = bp * qinv + bq * binv (mod n)
			bs := new(big.Int).Mul(bp, qinv)
			bs.Add(bs, new(big.Int).Mul(bq, pinv))
			bs.Mod(bs, n)
			writeToFile(partfile(i), bs.Bytes(), 0600)
		}
	}

	// spojeni souboru do jednoho
	if *joinFile != "" {
		max := msgscount()
		var bs []byte

		for i := 0; i<max; i++ {
			file := readFile(partfile(i))
			bs = bytes.Add(bs, file)
		}

		writeToFile(*joinFile, bs, 0600)
	}
}
