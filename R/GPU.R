#' @export
useGPU <- function(test_image) {
  if (!Rvision::isImage(test_image)) {
    stop("Invalid test image")
  }

  cpu_img <- Rvision::cloneImage(test_image)
  gpu_img <- Rvision::cloneImage(test_image)
  gpu <- tryCatch(
    {
      gpu_img$toGPU()
      TRUE
    },
    error = function(e) FALSE
  )

  if (gpu) {
    # Warm-up
    Rvision::not(cpu_img, target = "self")
    Rvision::subtract(cpu_img, cpu_img, cpu_img)
    Rvision::multiply(cpu_img, cpu_img, cpu_img)
    Rvision::compare(cpu_img, 50, ">=", cpu_img)

    Rvision::not(gpu_img, target = "self")
    Rvision::subtract(gpu_img, gpu_img, gpu_img)
    Rvision::multiply(gpu_img, gpu_img, gpu_img)
    Rvision::compare(gpu_img, 50, ">=", gpu_img)

    # Speed test
    sp1 <- system.time({
      Rvision::not(cpu_img, target = "self")
      Rvision::subtract(cpu_img, cpu_img, cpu_img)
      Rvision::multiply(cpu_img, cpu_img, cpu_img)
      Rvision::compare(cpu_img, 50, ">=", cpu_img)
    })

    sp2 <- system.time({
      Rvision::not(gpu_img, target = "self")
      Rvision::subtract(gpu_img, gpu_img, gpu_img)
      Rvision::multiply(gpu_img, gpu_img, gpu_img)
      Rvision::compare(gpu_img, 50, ">=", gpu_img)
    })

    unname(sp1[3] > sp2[3])
  } else {
    FALSE
  }
}
