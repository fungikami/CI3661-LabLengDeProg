
# Clase Circulo
class Circulo
  # b. Setter y getter de radio
  attr_reader :radio

  # c. Constructor
  def initialize(radio)
    self.radio = radio
  end

  # Setter de radio >= 0
  def radio=(valor)
    if valor < 0
      raise 'Radio invalido'
    else
      @radio = valor
    end
  end

  # d. Calcula el area del circulo
  def area
    Math::PI * @radio ** 2
  end
end

class Cilindro < Circulo
  # b. Setter y getter de altura
  attr_accessor :altura
  
  # c. Constructor
  def initialize(radio, altura)
    super(radio)
    self.altura = altura
  end
  
  # Setter de altura >= 0
  def altura=(valor)
    if valor < 0
      raise 'Altura invalida'
    else
      @altura = valor
    end
  end
  
  # d. Calcula el volumen del cilindro
  def volumen
    area * @altura
  end
end

# Ejemplo Circulo
circulo = Circulo.new(5)
puts circulo.area # => 78.53981633974483
circulo.radio = -2 # => RuntimeError: Radio invalido
circulo2 = Circulo.new(-2) # => RuntimeError: Radio invalido

# Ejemplo Cilindro
cilindro = Cilindro.new(5, 10)
puts cilindro.volumen # => 785.3981633974483
# cilindro.radio = -2 # => RuntimeError: Radio invalido
# cilindro.altura = -2 # => RuntimeError: Altura invalida
# cilindro2 = Cilindro.new(-2, -2) # => RuntimeError: Radio invalido

