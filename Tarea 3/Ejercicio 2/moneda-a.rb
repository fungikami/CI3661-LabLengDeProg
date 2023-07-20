class Moneda
  VALOR = 0.0

  def self.convertir(monto)
    monto / self::VALOR
  end
end

class Dolar < Moneda
  VALOR = 1.0
end

class Yen < Moneda
  VALOR = 0.00691269
end

class Euro < Moneda
  VALOR = 1.085482
end

class Bolivar < Moneda
  VALOR = 0.035738
end

class Bitcoin < Moneda
  VALOR = 31149.96
end

class Float
  def dolares
    Dolar.convertir(self)
  end

  def yens
    Yen.convertir(self)
  end

  def euros
    Euro.convertir(self)
  end

  def bolivares
    Bolivar.convertir(self)
  end

  def bitcoins
    Bitcoin.convertir(self)
  end
end

# Ejemplo de uso
dolaricos = 10.0
enEuros = dolaricos.euros
puts "#{dolaricos} dólares equivalen a #{enEuros} euros"
enYenes = dolaricos.yens
puts "#{dolaricos} dólares equivalen a #{enYenes} yenes"
enBolivares = dolaricos.bolivares
puts "#{dolaricos} dólares equivalen a #{enBolivares} bolívares"
enBitcoins = dolaricos.bitcoins
puts "#{dolaricos} dólares equivalen a #{enBitcoins} bitcoins"
