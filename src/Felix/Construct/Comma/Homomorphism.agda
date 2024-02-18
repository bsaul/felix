{-# OPTIONS --safe --without-K #-}

open import Level

open import Felix.Raw
open import Felix.Equiv
import Felix.Laws as L
open import Felix.Homomorphism

module Felix.Construct.Comma.Homomorphism
   {o₀}{obj₀ : Set o₀} {ℓ₀} (_⇨₀_ : obj₀ → obj₀ → Set ℓ₀) ⦃ _ : Category _⇨₀_ ⦄
   {o₁}{obj₁ : Set o₁} {ℓ₁} (_⇨₁_ : obj₁ → obj₁ → Set ℓ₁) ⦃ _ : Category _⇨₁_ ⦄
   {o₂}{obj₂ : Set o₂} {ℓ₂} (_⇨₂_ : obj₂ → obj₂ → Set ℓ₂) ⦃ _ : Category _⇨₂_ ⦄
   {q₀} ⦃ eq₀ : Equivalent q₀ _⇨₀_ ⦄ ⦃ _ : L.Category _⇨₀_ ⦄
   {q₁} ⦃ eq₁ : Equivalent q₁ _⇨₁_ ⦄ -- ⦃ _ : L.Category _⇨₁_ ⦄
   {q₂} ⦃ eq₂ : Equivalent q₂ _⇨₂_ ⦄ -- ⦃ _ : L.Category _⇨₂_ ⦄
   ⦃ _ : Homomorphismₒ obj₁ obj₀ ⦄ ⦃ _ : Homomorphism _⇨₁_ _⇨₀_ ⦄
     ⦃ catH₁ : CategoryH _⇨₁_ _⇨₀_ ⦄
   ⦃ _ : Homomorphismₒ obj₂ obj₀ ⦄ ⦃ _ : Homomorphism _⇨₂_ _⇨₀_ ⦄
     ⦃ catH₂ : CategoryH _⇨₂_ _⇨₀_ ⦄
 where

open import Felix.Construct.Comma.Raw _⇨₀_ _⇨₁_ _⇨₂_ public

module comma-homomorphism-instances where

  instance

    open import Data.Product using (_,_; proj₁; proj₂)
    open import Felix.Homomorphism

    categoryH₁ : CategoryH _↬_ _⇨₁_
    categoryH₁ = record { F-cong = proj₁ ; F-id = refl≈ ; F-∘ = refl≈ }
      where open ≈-Reasoning ⦃ eq₁ ⦄

    categoryH₂ : CategoryH _↬_ _⇨₂_
    categoryH₂ = record { F-cong = proj₂ ; F-id = refl≈ ; F-∘ = refl≈ }
      where open ≈-Reasoning ⦃ eq₂ ⦄

    -- Also CartesianH, CartesianClosedH, and LogicH
