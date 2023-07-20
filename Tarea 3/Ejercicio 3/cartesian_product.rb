def cartesian_product(collection1, collection2)
  collection1.each do |element1|
    collection2.each do |element2|
      yield [element1, element2]
    end
  end
end

array1 = [:a, :b, :c]
array2 = [4, 5]

cartesian_product(array1, array2) do |element|
  p element
end
