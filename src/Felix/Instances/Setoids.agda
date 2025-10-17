-- Setoid category

{-# OPTIONS --safe --without-K #-}

open import Level

module Felix.Instances.Setoids (ℓ : Level) where

open import Algebra.Core
open import Data.Empty.Polymorphic
open import Data.Product as × using ()
open import Data.Sum as ⊎ using ()
open import Data.Sum.Relation.Binary.Pointwise as P+ hiding (map)
open import Data.Product.Relation.Binary.Pointwise.NonDependent as P*
import Data.Unit as ⊤₀
open import Data.Unit.Polymorphic hiding (tt)
open import Function using (flip; Func; _⟨$⟩_; _⟶ₛ_)
import Function.Construct.Identity    as I
import Function.Construct.Composition as C
import Function.Construct.Constant    as K
open import Relation.Binary.Bundles using (Setoid)
open import Relation.Binary.Structures using (IsEquivalence)

-- Felix
open import Felix.Raw using 
         (Category; Products; Coproducts ; Cartesian; Cocartesian)

open Setoid
open Func
open IsEquivalence

private
  variable 
    A B C D : Setoid ℓ ℓ

-- infix 0 _⇨̇_
-- _⇨̇_ : ∀ (a b : Setoid o ℓ) → Setoid (o ⊔ ℓ) (o ⊔ ℓ)
-- _⇨̇_ = E._⇨_ where import Function.Relation.Binary.Setoid.Equality as E

pattern tt = lift ⊤₀.tt

-- Empty Setoid
𝟘 : Setoid ℓ ℓ
𝟘 .Carrier = ⊥
𝟘 ._≈_ = λ ()
𝟘 .isEquivalence .refl {()}
𝟘 .isEquivalence .sym {()}
𝟘 .isEquivalence .trans {()}

-- Unit Setoid
𝟙 : Setoid ℓ ℓ
𝟙 .Carrier = ⊤
𝟙 ._≈_ tt tt = ⊤
𝟙 .isEquivalence = _

absurd : Func 𝟘 A
absurd = record { to = λ () ; cong = λ { { () } } }

infix 2 _⊨_
pattern _⊨_ t c = record { to = t ; cong = c }

infixl 6 _+_
_+_ : Op₂ (Setoid ℓ ℓ)
_+_ = ⊎-setoid

infixl 7 _*_
_*_ : Op₂ (Setoid ℓ ℓ)
_*_ = ×-setoid

_▵_ : A ⟶ₛ B → A ⟶ₛ C → A ⟶ₛ B * C
(f ⊨ p) ▵ (g ⊨ q)  = ×.< f , g > ⊨ ×.< p , q >

_▿_ : A ⟶ₛ C → B ⟶ₛ C → A + B ⟶ₛ C
(f ⊨ p) ▿ (g ⊨ q) = ⊎.[ f , g ]′ ⊨ λ { (inj₁ x) → p x ; (inj₂ x) → q x }

module Setoid-instances where instance

  category : Category (_⟶ₛ_ {ℓ} {ℓ})
  category = record
    { id = λ {A} → I.function A
    ; _∘_ = flip C.function
    }

  products : Products (Setoid ℓ ℓ)
  products = record 
    { ⊤   = 𝟙
    ; _×_ = _*_ 
    }
  
  coproducts : Coproducts (Setoid ℓ ℓ)
  coproducts = record 
    { ⊥   = 𝟘 
    ; _⊎_ = _+_ 
    }

  cartesian : Cartesian _⟶ₛ_
  cartesian = record 
    { !   = K.function _ 𝟙 tt
    ; _▵_ =  _▵_
    ; exl = ×.proj₁ ⊨ ×.proj₁ 
    ; exr = ×.proj₂ ⊨ ×.proj₂ 
    }

  cocartesian : Cocartesian (_⟶ₛ_ {ℓ} {ℓ})
  cocartesian = record 
    { ¡   = absurd 
    ; _▿_ = _▿_ 
    ; inl = ⊎.inj₁ ⊨ inj₁ 
    ; inr = ⊎.inj₂ ⊨ inj₂ 
    }

