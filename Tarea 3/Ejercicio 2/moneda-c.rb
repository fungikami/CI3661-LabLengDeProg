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

  def comparar(otra_moneda)
    otra_moneda_valor = otra_moneda.valor * otra_moneda.class::TASA_CAMBIO
    comparar_con_valor(otra_moneda_valor)
  end

  def comparar_con_valor(valor_en_dolares)
    valor_moneda = @valor * self.class::TASA_CAMBIO
    if valor_moneda < valor_en_dolares
      :menor
    elsif valor_moneda == valor_en_dolares
      :igual
    else
      :mayor
    end
  end

  def self.tasa_cambio(moneda_origen, moneda_destino)
    moneda_origen::TASA_CAMBIO / moneda_destino::TASA_CAMBIO
  end

  def self.moneda_clase(moneda)
    MONEDAS[moneda]
  end

  TASA_CAMBIO = 1.0
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

class Numeric
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

# Ejemplo de uso:
puts 1.dolares.comparar(1.dolares) # => :igual
puts 1.dolares.comparar(2.dolares) # => :menor
puts 2.dolares.comparar(1.dolares) # => :mayor
puts 10.bolivares.comparar(2.dolares) # => :menor
puts 10000000.bolivares.comparar(2.dolares) # => :mayor
puts (2.euros.comparar(3.dolares)).to_s # => :menor
puts (2.euros.comparar(2.dolares)) # => :mayor