package com.github.codingdebugallday.driver.common.infra.annotations;

import java.util.Optional;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import org.springframework.context.ApplicationContext;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 自定义Validator校验驱动id参数是否合法
 * </p>
 *
 * @author isaac 2020/7/15 20:09
 * @since 1.0
 */
public class DriverIdValidator implements ConstraintValidator<DriverId, Long> {

    private static final PluginDriverSiteRepository PLUGIN_DRIVER_SITE_REPOSITORY;

    static {
        ApplicationContext context = Optional.ofNullable(ApplicationContextHelper.getContext())
                .orElseThrow(() ->
                        new DriverException("not spring env, cannot get ApplicationContext"));
        PLUGIN_DRIVER_SITE_REPOSITORY = context.getBean(PluginDriverSiteRepository.class);
    }

    @Override
    public boolean isValid(Long driverId, ConstraintValidatorContext constraintValidatorContext) {
        if (StringUtils.isEmpty(driverId)) {
            return true;
        }
        return PLUGIN_DRIVER_SITE_REPOSITORY.hashIsExist(String.valueOf(driverId));
    }

    @Override
    public void initialize(DriverId constraintAnnotation) {
        //
    }
}
