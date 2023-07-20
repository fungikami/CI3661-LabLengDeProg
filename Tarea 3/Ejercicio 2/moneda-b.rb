class Moneda
  def initialize(valor)
    @valor = valor
  end

  def valor
    @valor
  end

  def en(moneda_destino)
    destino_class = Moneda.moneda_clase(moneda_destino)
    tasa = Moneda.tasa_cambio(self.class, destino_class)
    destino_class.new(@valor / tasa)
  end

  def self.tasa_cambio(moneda_origen, moneda_destino)
    moneda_destino::TASA_CAMBIO / moneda_origen::TASA_CAMBIO
  end

  def self.moneda_clase(moneda)
    MONEDAS[moneda]
  end
end

class Dolar < Moneda
  TASA_CAMBIO = 1.0
end

class Yen < Moneda
  TASA_CAMBIO = 0.00691269
end

class Euro < Moneda
  TASA_CAMBIO = 1.085482
end

class Bolivar < Moneda
  TASA_CAMBIO = 0.035738
end

class Bitcoin < Moneda
  TASA_CAMBIO = 31149.96
end

Moneda::MONEDAS = {
  dolares: Dolar,
  yens: Yen,
  euros: Euro,
  bolivares: Bolivar,
  bitcoins: Bitcoin
}

class Float
  def dolares
    Dolar.new(self)
  end

  def yens
    Yen.new(self)
  end

  def euros
    Euro.new(self)
  end

  def bolivares
    Bolivar.new(self)
  end

  def bitcoins
    Bitcoin.new(self)
  end
end

# Ejemplo de uso
puts 15.0.dolares.en(:euros).valor.round(2)       # 13.82
puts 500.0.euros.en(:bolivares).valor.round(2)    # 15186.66
puts 10000.0.yens.en(:dolares).valor.round(2)     # 69.13
puts 1.5.bitcoins.en(:yens).valor.round(2)        # 6759299.2
puts 100.0.bolivares.en(:bitcoins).valor.round(2) # 0.0
puts 100.0.dolares.en(:bolivares).valor.round(2)  # 2798.14